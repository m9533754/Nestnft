
(define-non-fungible-token pension-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-nft-not-found (err u103))
(define-constant err-listing-expired (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-invalid-pension-data (err u106))
(define-constant err-pension-not-vested (err u107))
(define-constant err-pension-already-claimed (err u108))
(define-constant err-listing-not-active (err u109))
(define-constant err-bid-too-low (err u110))
(define-constant err-bid-not-found (err u111))
(define-constant err-auction-ended (err u112))
(define-constant err-auction-not-ended (err u113))
(define-constant err-payment-failed (err u114))
(define-constant err-not-bidder (err u115))
(define-constant err-cannot-bid-own-nft (err u116))

(define-data-var last-token-id uint u0)
(define-data-var total-pension-value uint u0)
(define-data-var marketplace-fee-percentage uint u250)

(define-map token-count principal uint)

(define-map pension-metadata uint {
    employer: (string-ascii 50),
    employee: principal,
    monthly-benefit: uint,
    vesting-date: uint,
    retirement-age: uint,
    contribution-years: uint,
    pension-type: (string-ascii 20),
    is-vested: bool,
    is-claimed: bool,
    created-at: uint
})

(define-map pension-transfers uint {
    from: principal,
    to: principal,
    transfer-date: uint,
    transfer-price: uint
})

(define-map employee-pensions principal (list 50 uint))

(define-map pension-valuations uint {
    current-value: uint,
    last-updated: uint,
    valuation-method: (string-ascii 30)
})

(define-map marketplace-listings uint {
    seller: principal,
    starting-price: uint,
    current-highest-bid: uint,
    current-highest-bidder: (optional principal),
    auction-end: uint,
    is-active: bool,
    created-at: uint
})

(define-map marketplace-bids {token-id: uint, bidder: principal} {
    bid-amount: uint,
    bid-time: uint,
    is-active: bool
})

(define-map bidder-deposits principal uint)

(define-public (mint-pension-nft 
    (recipient principal)
    (employer (string-ascii 50))
    (monthly-benefit uint)
    (vesting-date uint)
    (retirement-age uint)
    (contribution-years uint)
    (pension-type (string-ascii 20)))
    (let 
        (
            (token-id (+ (var-get last-token-id) u1))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> monthly-benefit u0) err-invalid-pension-data)
        (asserts! (> retirement-age u0) err-invalid-pension-data)
        (asserts! (> contribution-years u0) err-invalid-pension-data)
        
        (try! (nft-mint? pension-nft token-id recipient))
        
        (map-set pension-metadata token-id {
            employer: employer,
            employee: recipient,
            monthly-benefit: monthly-benefit,
            vesting-date: vesting-date,
            retirement-age: retirement-age,
            contribution-years: contribution-years,
            pension-type: pension-type,
            is-vested: (>= stacks-block-height vesting-date),
            is-claimed: false,
            created-at: stacks-block-height
        })
        
        (map-set pension-valuations token-id {
            current-value: (* monthly-benefit u12 u20),
            last-updated: stacks-block-height,
            valuation-method: "basic-calculation"
        })
        
        (let ((current-pensions (default-to (list) (map-get? employee-pensions recipient))))
            (map-set employee-pensions recipient (unwrap! (as-max-len? (append current-pensions token-id) u50) err-invalid-pension-data))
        )
        
        (map-set token-count recipient (+ (default-to u0 (map-get? token-count recipient)) u1))
        (var-set last-token-id token-id)
        (var-set total-pension-value (+ (var-get total-pension-value) (* monthly-benefit u12 u20)))
        
        (ok token-id)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (asserts! (is-some (nft-get-owner? pension-nft token-id)) err-nft-not-found)
        
        (map-set pension-transfers token-id {
            from: sender,
            to: recipient,
            transfer-date: stacks-block-height,
            transfer-price: u0
        })
        
        (let ((sender-pensions (default-to (list) (map-get? employee-pensions sender)))
              (recipient-pensions (default-to (list) (map-get? employee-pensions recipient))))
            (map-set employee-pensions sender (filter-pension-list sender-pensions token-id))
            (map-set employee-pensions recipient (unwrap! (as-max-len? (append recipient-pensions token-id) u50) err-invalid-pension-data))
        )
        
        (map-set token-count sender (- (default-to u0 (map-get? token-count sender)) u1))
        (map-set token-count recipient (+ (default-to u0 (map-get? token-count recipient)) u1))
        
        (nft-transfer? pension-nft token-id sender recipient)
    )
)

(define-public (update-vesting-status (token-id uint))
    (let ((pension-data (unwrap! (map-get? pension-metadata token-id) err-nft-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set pension-metadata token-id (merge pension-data {
            is-vested: (>= stacks-block-height (get vesting-date pension-data))
        }))
        
        (ok true)
    )
)

(define-public (claim-pension (token-id uint))
    (let ((pension-data (unwrap! (map-get? pension-metadata token-id) err-nft-not-found))
          (token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (get is-vested pension-data) err-pension-not-vested)
        (asserts! (not (get is-claimed pension-data)) err-pension-already-claimed)
        
        (map-set pension-metadata token-id (merge pension-data {
            is-claimed: true
        }))
        
        (ok (get monthly-benefit pension-data))
    )
)

(define-public (update-pension-valuation (token-id uint) (new-value uint) (method (string-ascii 30)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some (map-get? pension-metadata token-id)) err-nft-not-found)
        
        (map-set pension-valuations token-id {
            current-value: new-value,
            last-updated: stacks-block-height,
            valuation-method: method
        })
        
        (ok true)
    )
)

(define-private (filter-pension-list (pension-list (list 50 uint)) (token-id uint))
    (filter is-not-target-pension pension-list)
)

(define-private (is-not-target-pension (id uint))
    true
)

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? pension-nft token-id))
)

(define-read-only (get-pension-metadata (token-id uint))
    (map-get? pension-metadata token-id)
)

(define-read-only (get-pension-valuation (token-id uint))
    (map-get? pension-valuations token-id)
)

(define-read-only (get-employee-pensions (employee principal))
    (map-get? employee-pensions employee)
)

(define-read-only (get-pension-transfer-history (token-id uint))
    (map-get? pension-transfers token-id)
)

(define-read-only (get-total-pension-value)
    (var-get total-pension-value)
)

(define-read-only (get-token-count (owner principal))
    (default-to u0 (map-get? token-count owner))
)

(define-read-only (is-pension-vested (token-id uint))
    (match (map-get? pension-metadata token-id)
        pension-data (ok (get is-vested pension-data))
        (err err-nft-not-found)
    )
)

(define-read-only (is-pension-claimed (token-id uint))
    (match (map-get? pension-metadata token-id)
        pension-data (ok (get is-claimed pension-data))
        (err err-nft-not-found)
    )
)

(define-read-only (calculate-pension-value (token-id uint))
    (match (map-get? pension-metadata token-id)
        pension-data 
            (let ((monthly-benefit (get monthly-benefit pension-data))
                  (years-remaining (if (> (get retirement-age pension-data) u65) u20 u15)))
                (ok (* monthly-benefit u12 years-remaining))
            )
        (err err-nft-not-found)
    )
)

(define-read-only (get-contract-stats)
    (ok {
        total-nfts: (var-get last-token-id),
        total-value: (var-get total-pension-value),
        contract-owner: contract-owner
    })
)

(define-public (create-marketplace-listing (token-id uint) (starting-price uint) (auction-duration uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (auction-end (+ stacks-block-height auction-duration)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (> starting-price u0) err-insufficient-payment)
        (asserts! (> auction-duration u0) err-invalid-pension-data)
        (asserts! (is-none (map-get? marketplace-listings token-id)) err-listing-expired)
        
        (map-set marketplace-listings token-id {
            seller: tx-sender,
            starting-price: starting-price,
            current-highest-bid: u0,
            current-highest-bidder: none,
            auction-end: auction-end,
            is-active: true,
            created-at: stacks-block-height
        })
        
        (ok token-id)
    )
)

(define-public (place-bid (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
          (bid-amount (stx-get-balance tx-sender))
          (current-highest-bid (get current-highest-bid listing))
          (current-highest-bidder (get current-highest-bidder listing)))
        
        (asserts! (get is-active listing) err-listing-not-active)
        (asserts! (< stacks-block-height (get auction-end listing)) err-auction-ended)
        (asserts! (not (is-eq tx-sender (get seller listing))) err-cannot-bid-own-nft)
        (asserts! (> bid-amount (get starting-price listing)) err-bid-too-low)
        (asserts! (> bid-amount current-highest-bid) err-bid-too-low)
        (asserts! (> bid-amount u0) err-insufficient-payment)
        
        (match current-highest-bidder
            previous-bidder
                (begin
                    (try! (stx-transfer? current-highest-bid contract-owner previous-bidder))
                    (map-set bidder-deposits previous-bidder (- (default-to u0 (map-get? bidder-deposits previous-bidder)) current-highest-bid))
                )
            true
        )
        
        (try! (stx-transfer? bid-amount tx-sender contract-owner))
        (map-set bidder-deposits tx-sender (+ (default-to u0 (map-get? bidder-deposits tx-sender)) bid-amount))
        
        (map-set marketplace-bids {token-id: token-id, bidder: tx-sender} {
            bid-amount: bid-amount,
            bid-time: stacks-block-height,
            is-active: true
        })
        
        (map-set marketplace-listings token-id (merge listing {
            current-highest-bid: bid-amount,
            current-highest-bidder: (some tx-sender)
        }))
        
        (ok bid-amount)
    )
)

(define-public (finalize-auction (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
          (token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (highest-bidder (get current-highest-bidder listing))
          (highest-bid (get current-highest-bid listing))
          (seller (get seller listing))
          (marketplace-fee (/ (* highest-bid (var-get marketplace-fee-percentage)) u10000)))
        
        (asserts! (get is-active listing) err-listing-not-active)
        (asserts! (>= stacks-block-height (get auction-end listing)) err-auction-not-ended)
        (asserts! (or (is-eq tx-sender seller) (is-eq tx-sender contract-owner)) err-not-token-owner)
        
        (match highest-bidder
            winner
                (begin
                    (try! (nft-transfer? pension-nft token-id seller winner))
                    (try! (stx-transfer? (- highest-bid marketplace-fee) contract-owner seller))
                    (map-set bidder-deposits winner (- (default-to u0 (map-get? bidder-deposits winner)) highest-bid))
                    
                    (map-set pension-transfers token-id {
                        from: seller,
                        to: winner,
                        transfer-date: stacks-block-height,
                        transfer-price: highest-bid
                    })
                    
                    (let ((seller-pensions (default-to (list) (map-get? employee-pensions seller)))
                          (winner-pensions (default-to (list) (map-get? employee-pensions winner))))
                        (map-set employee-pensions seller (filter-pension-list seller-pensions token-id))
                        (map-set employee-pensions winner (unwrap! (as-max-len? (append winner-pensions token-id) u50) err-invalid-pension-data))
                    )
                    
                    (map-set token-count seller (- (default-to u0 (map-get? token-count seller)) u1))
                    (map-set token-count winner (+ (default-to u0 (map-get? token-count winner)) u1))
                    
                    (map-set marketplace-listings token-id (merge listing {
                        is-active: false
                    }))
                    
                    (ok highest-bid)
                )
            (begin
                (map-set marketplace-listings token-id (merge listing {
                    is-active: false
                }))
                (ok u0)
            )
        )
    )
)

(define-public (cancel-listing (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
          (current-highest-bidder (get current-highest-bidder listing))
          (current-highest-bid (get current-highest-bid listing)))
        
        (asserts! (is-eq tx-sender (get seller listing)) err-not-token-owner)
        (asserts! (get is-active listing) err-listing-not-active)
        (asserts! (< stacks-block-height (get auction-end listing)) err-auction-ended)
        
        (match current-highest-bidder
            bidder
                (begin
                    (try! (stx-transfer? current-highest-bid contract-owner bidder))
                    (map-set bidder-deposits bidder (- (default-to u0 (map-get? bidder-deposits bidder)) current-highest-bid))
                )
            true
        )
        
        (map-set marketplace-listings token-id (merge listing {
            is-active: false
        }))
        
        (ok true)
    )
)

(define-public (withdraw-failed-bid (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-listing-not-found))
          (bid-info (unwrap! (map-get? marketplace-bids {token-id: token-id, bidder: tx-sender}) err-bid-not-found))
          (current-highest-bidder (get current-highest-bidder listing)))
        
        (asserts! (not (get is-active listing)) err-listing-not-active)
        (asserts! (get is-active bid-info) err-bid-not-found)
        (asserts! (not (is-eq (some tx-sender) current-highest-bidder)) err-not-bidder)
        
        (let ((bid-amount (get bid-amount bid-info))
              (current-deposit (default-to u0 (map-get? bidder-deposits tx-sender))))
            (asserts! (>= current-deposit bid-amount) err-insufficient-payment)
            
            (try! (stx-transfer? bid-amount contract-owner tx-sender))
            (map-set bidder-deposits tx-sender (- current-deposit bid-amount))
            
            (map-set marketplace-bids {token-id: token-id, bidder: tx-sender} (merge bid-info {
                is-active: false
            }))
            
            (ok bid-amount)
        )
    )
)

(define-public (update-marketplace-fee (new-fee-percentage uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee-percentage u1000) err-invalid-pension-data)
        
        (var-set marketplace-fee-percentage new-fee-percentage)
        (ok new-fee-percentage)
    )
)

(define-read-only (get-marketplace-listing (token-id uint))
    (map-get? marketplace-listings token-id)
)

(define-read-only (get-marketplace-bid (token-id uint) (bidder principal))
    (map-get? marketplace-bids {token-id: token-id, bidder: bidder})
)

(define-read-only (get-bidder-deposit (bidder principal))
    (default-to u0 (map-get? bidder-deposits bidder))
)

(define-read-only (get-marketplace-fee-percentage)
    (var-get marketplace-fee-percentage)
)

(define-read-only (calculate-marketplace-fee (sale-price uint))
    (ok (/ (* sale-price (var-get marketplace-fee-percentage)) u10000))
)

(define-read-only (get-active-listings)
    (ok (list
        (map-get? marketplace-listings u1)
        (map-get? marketplace-listings u2)
        (map-get? marketplace-listings u3)
        (map-get? marketplace-listings u4)
        (map-get? marketplace-listings u5)
    ))
)

(define-read-only (is-auction-ended (token-id uint))
    (match (map-get? marketplace-listings token-id)
        listing (ok (>= stacks-block-height (get auction-end listing)))
        (err err-listing-not-found)
    )
)