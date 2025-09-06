
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
(define-constant err-beneficiary-not-found (err u117))
(define-constant err-beneficiary-already-exists (err u118))
(define-constant err-not-beneficiary (err u119))
(define-constant err-inheritance-not-claimable (err u120))
(define-constant err-beneficiary-limit-reached (err u121))
(define-constant err-cannot-designate-self (err u122))
(define-constant err-inheritance-period-not-met (err u123))
(define-constant err-death-certificate-required (err u124))
(define-constant err-beneficiary-verification-failed (err u125))

(define-data-var last-token-id uint u0)
(define-data-var total-pension-value uint u0)
(define-data-var marketplace-fee-percentage uint u250)
(define-data-var inheritance-waiting-period uint u4320)
(define-data-var max-beneficiaries-per-token uint u5)

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

(define-map pension-beneficiaries uint {
    primary-beneficiary: (optional principal),
    secondary-beneficiaries: (list 4 principal),
    inheritance-percentage: (list 5 uint),
    beneficiary-count: uint,
    designation-date: uint,
    last-updated: uint
})

(define-map beneficiary-verification {token-id: uint, beneficiary: principal} {
    verification-status: (string-ascii 20),
    verification-date: uint,
    death-certificate-submitted: bool,
    approved-by: (optional principal),
    inheritance-claim-date: (optional uint)
})

(define-map inheritance-claims uint {
    claimant: principal,
    claim-date: uint,
    waiting-period-end: uint,
    claim-status: (string-ascii 20),
    verification-required: bool,
    contested: bool
})

(define-map beneficiary-rights {token-id: uint, beneficiary: principal} {
    inheritance-percentage: uint,
    designation-date: uint,
    is-active: bool,
    priority-level: uint
})

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
        
        (map-set pension-beneficiaries token-id {
            primary-beneficiary: none,
            secondary-beneficiaries: (list),
            inheritance-percentage: (list u100),
            beneficiary-count: u0,
            designation-date: stacks-block-height,
            last-updated: stacks-block-height
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
        
        ;; Check if multi-sig governance is required
        (match (map-get? multisig-config token-id)
            config
                (if (get is-active config)
                    ;; Multi-sig is active - this transfer must be via executed proposal
                    (asserts! false err-multisig-required)
                    ;; Multi-sig not active - proceed normally
                    true
                )
            ;; No multi-sig config - proceed normally  
            true
        )
        
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

(define-public (designate-primary-beneficiary (token-id uint) (beneficiary principal) (inheritance-percentage uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (current-beneficiaries (default-to {
              primary-beneficiary: none,
              secondary-beneficiaries: (list),
              inheritance-percentage: (list),
              beneficiary-count: u0,
              designation-date: stacks-block-height,
              last-updated: stacks-block-height
          } (map-get? pension-beneficiaries token-id))))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (not (is-eq tx-sender beneficiary)) err-cannot-designate-self)
        (asserts! (> inheritance-percentage u0) err-invalid-pension-data)
        (asserts! (<= inheritance-percentage u100) err-invalid-pension-data)
        
        ;; Check if multi-sig governance is required
        (match (map-get? multisig-config token-id)
            config
                (if (get is-active config)
                    ;; Multi-sig is active - this action must be via executed proposal
                    (asserts! false err-multisig-required)
                    ;; Multi-sig not active - proceed normally
                    true
                )
            ;; No multi-sig config - proceed normally  
            true
        )
        
        (map-set pension-beneficiaries token-id (merge current-beneficiaries {
            primary-beneficiary: (some beneficiary),
            inheritance-percentage: (list inheritance-percentage),
            beneficiary-count: u1,
            last-updated: stacks-block-height
        }))
        
        (map-set beneficiary-rights {token-id: token-id, beneficiary: beneficiary} {
            inheritance-percentage: inheritance-percentage,
            designation-date: stacks-block-height,
            is-active: true,
            priority-level: u1
        })
        
        (ok beneficiary)
    )
)

(define-public (add-secondary-beneficiary (token-id uint) (beneficiary principal) (inheritance-percentage uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (current-beneficiaries (unwrap! (map-get? pension-beneficiaries token-id) err-beneficiary-not-found))
          (secondary-list (get secondary-beneficiaries current-beneficiaries))
          (percentage-list (get inheritance-percentage current-beneficiaries))
          (current-count (get beneficiary-count current-beneficiaries)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (not (is-eq tx-sender beneficiary)) err-cannot-designate-self)
        (asserts! (< current-count (var-get max-beneficiaries-per-token)) err-beneficiary-limit-reached)
        (asserts! (> inheritance-percentage u0) err-invalid-pension-data)
        (asserts! (<= inheritance-percentage u100) err-invalid-pension-data)
        
        (let ((total-percentage (fold + percentage-list u0)))
            (asserts! (<= (+ total-percentage inheritance-percentage) u100) err-invalid-pension-data)
        )
        
        (map-set pension-beneficiaries token-id (merge current-beneficiaries {
            secondary-beneficiaries: (unwrap! (as-max-len? (append secondary-list beneficiary) u4) err-beneficiary-limit-reached),
            inheritance-percentage: (unwrap! (as-max-len? (append percentage-list inheritance-percentage) u5) err-invalid-pension-data),
            beneficiary-count: (+ current-count u1),
            last-updated: stacks-block-height
        }))
        
        (map-set beneficiary-rights {token-id: token-id, beneficiary: beneficiary} {
            inheritance-percentage: inheritance-percentage,
            designation-date: stacks-block-height,
            is-active: true,
            priority-level: (+ current-count u1)
        })
        
        (ok beneficiary)
    )
)

(define-public (update-beneficiary-percentage (token-id uint) (beneficiary principal) (new-percentage uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (beneficiary-info (unwrap! (map-get? beneficiary-rights {token-id: token-id, beneficiary: beneficiary}) err-beneficiary-not-found))
          (current-beneficiaries (unwrap! (map-get? pension-beneficiaries token-id) err-beneficiary-not-found)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (get is-active beneficiary-info) err-beneficiary-not-found)
        (asserts! (> new-percentage u0) err-invalid-pension-data)
        (asserts! (<= new-percentage u100) err-invalid-pension-data)
        
        (map-set beneficiary-rights {token-id: token-id, beneficiary: beneficiary} (merge beneficiary-info {
            inheritance-percentage: new-percentage
        }))
        
        (map-set pension-beneficiaries token-id (merge current-beneficiaries {
            last-updated: stacks-block-height
        }))
        
        (ok new-percentage)
    )
)

(define-public (remove-beneficiary (token-id uint) (beneficiary principal))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (beneficiary-info (unwrap! (map-get? beneficiary-rights {token-id: token-id, beneficiary: beneficiary}) err-beneficiary-not-found))
          (current-beneficiaries (unwrap! (map-get? pension-beneficiaries token-id) err-beneficiary-not-found)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (get is-active beneficiary-info) err-beneficiary-not-found)
        
        (map-set beneficiary-rights {token-id: token-id, beneficiary: beneficiary} (merge beneficiary-info {
            is-active: false
        }))
        
        (map-set pension-beneficiaries token-id (merge current-beneficiaries {
            beneficiary-count: (- (get beneficiary-count current-beneficiaries) u1),
            last-updated: stacks-block-height
        }))
        
        (ok true)
    )
)

(define-public (initiate-inheritance-claim (token-id uint))
    (let ((beneficiary-info (unwrap! (map-get? beneficiary-rights {token-id: token-id, beneficiary: tx-sender}) err-not-beneficiary))
          (current-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (waiting-period-end (+ stacks-block-height (var-get inheritance-waiting-period))))
        
        (asserts! (get is-active beneficiary-info) err-not-beneficiary)
        (asserts! (is-none (map-get? inheritance-claims token-id)) err-inheritance-not-claimable)
        
        (map-set inheritance-claims token-id {
            claimant: tx-sender,
            claim-date: stacks-block-height,
            waiting-period-end: waiting-period-end,
            claim-status: "pending",
            verification-required: true,
            contested: false
        })
        
        (map-set beneficiary-verification {token-id: token-id, beneficiary: tx-sender} {
            verification-status: "pending",
            verification-date: stacks-block-height,
            death-certificate-submitted: false,
            approved-by: none,
            inheritance-claim-date: (some stacks-block-height)
        })
        
        (ok waiting-period-end)
    )
)

(define-public (submit-death-certificate (token-id uint) (beneficiary principal))
    (let ((verification-info (unwrap! (map-get? beneficiary-verification {token-id: token-id, beneficiary: beneficiary}) err-beneficiary-not-found))
          (claim-info (unwrap! (map-get? inheritance-claims token-id) err-inheritance-not-claimable)))
        
        (asserts! (is-eq tx-sender beneficiary) err-not-beneficiary)
        (asserts! (is-eq (get claimant claim-info) beneficiary) err-not-beneficiary)
        
        (map-set beneficiary-verification {token-id: token-id, beneficiary: beneficiary} (merge verification-info {
            death-certificate-submitted: true,
            verification-status: "cert-submitted"
        }))
        
        (ok true)
    )
)

(define-public (approve-inheritance-claim (token-id uint) (beneficiary principal))
    (let ((verification-info (unwrap! (map-get? beneficiary-verification {token-id: token-id, beneficiary: beneficiary}) err-beneficiary-not-found))
          (claim-info (unwrap! (map-get? inheritance-claims token-id) err-inheritance-not-claimable)))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get death-certificate-submitted verification-info) err-death-certificate-required)
        (asserts! (is-eq (get claimant claim-info) beneficiary) err-not-beneficiary)
        
        (map-set beneficiary-verification {token-id: token-id, beneficiary: beneficiary} (merge verification-info {
            verification-status: "approved",
            approved-by: (some tx-sender)
        }))
        
        (map-set inheritance-claims token-id (merge claim-info {
            claim-status: "approved",
            verification-required: false
        }))
        
        (ok true)
    )
)

(define-public (finalize-inheritance-transfer (token-id uint))
    (let ((claim-info (unwrap! (map-get? inheritance-claims token-id) err-inheritance-not-claimable))
          (beneficiary (get claimant claim-info))
          (current-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (verification-info (unwrap! (map-get? beneficiary-verification {token-id: token-id, beneficiary: beneficiary}) err-beneficiary-verification-failed)))
        
        (asserts! (is-eq tx-sender beneficiary) err-not-beneficiary)
        (asserts! (is-eq (get claim-status claim-info) "approved") err-inheritance-not-claimable)
        (asserts! (>= stacks-block-height (get waiting-period-end claim-info)) err-inheritance-period-not-met)
        (asserts! (not (get verification-required claim-info)) err-beneficiary-verification-failed)
        (asserts! (is-eq (get verification-status verification-info) "approved") err-beneficiary-verification-failed)
        
        (try! (nft-transfer? pension-nft token-id current-owner beneficiary))
        
        (map-set pension-transfers token-id {
            from: current-owner,
            to: beneficiary,
            transfer-date: stacks-block-height,
            transfer-price: u0
        })
        
        (let ((owner-pensions (default-to (list) (map-get? employee-pensions current-owner)))
              (beneficiary-pensions (default-to (list) (map-get? employee-pensions beneficiary))))
            (map-set employee-pensions current-owner (filter-pension-list owner-pensions token-id))
            (map-set employee-pensions beneficiary (unwrap! (as-max-len? (append beneficiary-pensions token-id) u50) err-invalid-pension-data))
        )
        
        (map-set token-count current-owner (- (default-to u0 (map-get? token-count current-owner)) u1))
        (map-set token-count beneficiary (+ (default-to u0 (map-get? token-count beneficiary)) u1))
        
        (map-set inheritance-claims token-id (merge claim-info {
            claim-status: "completed"
        }))
        
        (ok beneficiary)
    )
)

(define-public (contest-inheritance-claim (token-id uint) (reason (string-ascii 100)))
    (let ((claim-info (unwrap! (map-get? inheritance-claims token-id) err-inheritance-not-claimable))
          (current-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found)))
        
        (asserts! (or (is-eq tx-sender current-owner) (is-eq tx-sender contract-owner)) err-not-token-owner)
        (asserts! (is-eq (get claim-status claim-info) "pending") err-inheritance-not-claimable)
        (asserts! (< stacks-block-height (get waiting-period-end claim-info)) err-inheritance-period-not-met)
        
        (map-set inheritance-claims token-id (merge claim-info {
            claim-status: "contested",
            contested: true
        }))
        
        (ok true)
    )
)

(define-public (emergency-override-inheritance (token-id uint) (new-owner principal))
    (let ((current-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found)))
        
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (try! (nft-transfer? pension-nft token-id current-owner new-owner))
        
        (map-set pension-transfers token-id {
            from: current-owner,
            to: new-owner,
            transfer-date: stacks-block-height,
            transfer-price: u0
        })
        
        (let ((owner-pensions (default-to (list) (map-get? employee-pensions current-owner)))
              (new-owner-pensions (default-to (list) (map-get? employee-pensions new-owner))))
            (map-set employee-pensions current-owner (filter-pension-list owner-pensions token-id))
            (map-set employee-pensions new-owner (unwrap! (as-max-len? (append new-owner-pensions token-id) u50) err-invalid-pension-data))
        )
        
        (map-set token-count current-owner (- (default-to u0 (map-get? token-count current-owner)) u1))
        (map-set token-count new-owner (+ (default-to u0 (map-get? token-count new-owner)) u1))
        
        (ok new-owner)
    )
)

(define-public (update-inheritance-waiting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-period u0) err-invalid-pension-data)
        
        (var-set inheritance-waiting-period new-period)
        (ok new-period)
    )
)

(define-public (update-max-beneficiaries (new-max uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-max u0) err-invalid-pension-data)
        (asserts! (<= new-max u10) err-invalid-pension-data)
        
        (var-set max-beneficiaries-per-token new-max)
        (ok new-max)
    )
)

(define-read-only (get-pension-beneficiaries (token-id uint))
    (map-get? pension-beneficiaries token-id)
)

(define-read-only (get-beneficiary-rights (token-id uint) (beneficiary principal))
    (map-get? beneficiary-rights {token-id: token-id, beneficiary: beneficiary})
)

(define-read-only (get-beneficiary-verification (token-id uint) (beneficiary principal))
    (map-get? beneficiary-verification {token-id: token-id, beneficiary: beneficiary})
)

(define-read-only (get-inheritance-claim (token-id uint))
    (map-get? inheritance-claims token-id)
)

(define-read-only (get-inheritance-waiting-period)
    (var-get inheritance-waiting-period)
)

(define-read-only (get-max-beneficiaries-per-token)
    (var-get max-beneficiaries-per-token)
)

(define-read-only (is-eligible-beneficiary (token-id uint) (beneficiary principal))
    (match (map-get? beneficiary-rights {token-id: token-id, beneficiary: beneficiary})
        rights (ok (get is-active rights))
        (ok false)
    )
)

(define-read-only (calculate-inheritance-value (token-id uint) (beneficiary principal))
    (match (map-get? beneficiary-rights {token-id: token-id, beneficiary: beneficiary})
        rights 
            (match (map-get? pension-valuations token-id)
                valuation 
                    (let ((total-value (get current-value valuation))
                          (percentage (get inheritance-percentage rights)))
                        (ok (/ (* total-value percentage) u100))
                    )
                (err err-nft-not-found)
            )
        (err err-beneficiary-not-found)
    )
)

(define-read-only (get-beneficiary-summary (token-id uint))
    (match (map-get? pension-beneficiaries token-id)
        beneficiaries
            (ok {
                primary-beneficiary: (get primary-beneficiary beneficiaries),
                beneficiary-count: (get beneficiary-count beneficiaries),
                designation-date: (get designation-date beneficiaries),
                last-updated: (get last-updated beneficiaries)
            })
        (err err-nft-not-found)
    )
)

;; Multi-Signature Governance System
;; Allows pension holders to require multiple approvals for critical operations

(define-constant err-invalid-threshold (err u126))
(define-constant err-not-co-signer (err u127))
(define-constant err-proposal-not-found (err u128))
(define-constant err-proposal-already-executed (err u129))
(define-constant err-proposal-expired (err u130))
(define-constant err-already-approved (err u131))
(define-constant err-insufficient-approvals (err u132))
(define-constant err-multisig-required (err u133))

(define-data-var proposal-counter uint u0)

;; Multi-sig configuration for each pension token
(define-map multisig-config uint {
    co-signers: (list 5 principal),
    threshold: uint,
    is-active: bool,
    created-at: uint
})

;; Proposal tracking
(define-map governance-proposals uint {
    token-id: uint,
    proposer: principal,
    action-type: (string-ascii 30),
    target-address: (optional principal),
    proposal-data: (string-ascii 200),
    approvals: (list 5 principal),
    approval-count: uint,
    is-executed: bool,
    created-at: uint,
    expires-at: uint
})

;; Track individual approvals
(define-map proposal-approvals {proposal-id: uint, signer: principal} bool)

;; Setup multi-signature governance for a pension token
(define-public (setup-multisig-governance (token-id uint) (co-signers (list 5 principal)) (threshold uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (signer-count (len co-signers)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (> threshold u1) err-invalid-threshold)
        (asserts! (<= threshold signer-count) err-invalid-threshold)
        (asserts! (> signer-count u1) err-invalid-threshold)
        
        (map-set multisig-config token-id {
            co-signers: co-signers,
            threshold: threshold,
            is-active: true,
            created-at: stacks-block-height
        })
        
        (ok true)
    )
)

;; Propose an action that requires multi-sig approval
(define-public (propose-action (token-id uint) (action-type (string-ascii 30)) (target-address (optional principal)) (proposal-data (string-ascii 200)))
    (let ((multisig (unwrap! (map-get? multisig-config token-id) err-nft-not-found))
          (proposal-id (+ (var-get proposal-counter) u1)))
        
        (asserts! (get is-active multisig) err-multisig-required)
        (asserts! (is-some (index-of (get co-signers multisig) tx-sender)) err-not-co-signer)
        
        (map-set governance-proposals proposal-id {
            token-id: token-id,
            proposer: tx-sender,
            action-type: action-type,
            target-address: target-address,
            proposal-data: proposal-data,
            approvals: (list tx-sender),
            approval-count: u1,
            is-executed: false,
            created-at: stacks-block-height,
            expires-at: (+ stacks-block-height u1440) ;; 24 hours
        })
        
        (map-set proposal-approvals {proposal-id: proposal-id, signer: tx-sender} true)
        (var-set proposal-counter proposal-id)
        
        (ok proposal-id)
    )
)

;; Approve a pending proposal
(define-public (approve-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) err-proposal-not-found))
          (token-id (get token-id proposal))
          (multisig (unwrap! (map-get? multisig-config token-id) err-nft-not-found))
          (existing-approval (default-to false (map-get? proposal-approvals {proposal-id: proposal-id, signer: tx-sender}))))
        
        (asserts! (get is-active multisig) err-multisig-required)
        (asserts! (is-some (index-of (get co-signers multisig) tx-sender)) err-not-co-signer)
        (asserts! (not (get is-executed proposal)) err-proposal-already-executed)
        (asserts! (< stacks-block-height (get expires-at proposal)) err-proposal-expired)
        (asserts! (not existing-approval) err-already-approved)
        
        (let ((new-approvals (unwrap! (as-max-len? (append (get approvals proposal) tx-sender) u5) err-invalid-pension-data))
              (new-count (+ (get approval-count proposal) u1)))
            
            (map-set governance-proposals proposal-id (merge proposal {
                approvals: new-approvals,
                approval-count: new-count
            }))
            
            (map-set proposal-approvals {proposal-id: proposal-id, signer: tx-sender} true)
            
            (ok new-count)
        )
    )
)

;; Execute an approved action
(define-public (execute-approved-action (proposal-id uint))
    (let ((proposal (unwrap! (map-get? governance-proposals proposal-id) err-proposal-not-found))
          (token-id (get token-id proposal))
          (multisig (unwrap! (map-get? multisig-config token-id) err-nft-not-found)))
        
        (asserts! (not (get is-executed proposal)) err-proposal-already-executed)
        (asserts! (< stacks-block-height (get expires-at proposal)) err-proposal-expired)
        (asserts! (>= (get approval-count proposal) (get threshold multisig)) err-insufficient-approvals)
        (asserts! (is-some (index-of (get co-signers multisig) tx-sender)) err-not-co-signer)
        
        (map-set governance-proposals proposal-id (merge proposal {
            is-executed: true
        }))
        
        (ok (get action-type proposal))
    )
)

;; Check if an action requires multi-sig approval
(define-read-only (requires-multisig-approval (token-id uint))
    (match (map-get? multisig-config token-id)
        config (ok (get is-active config))
        (ok false)
    )
)

;; Get multi-sig configuration for a token
(define-read-only (get-multisig-config (token-id uint))
    (map-get? multisig-config token-id)
)

;; Get proposal details
(define-read-only (get-proposal-status (proposal-id uint))
    (map-get? governance-proposals proposal-id)
)

;; Check if a principal is a co-signer for a token
(define-read-only (is-co-signer (token-id uint) (signer principal))
    (match (map-get? multisig-config token-id)
        config (ok (is-some (index-of (get co-signers config) signer)))
        (ok false)
    )
)

;; Get approval progress for a proposal
(define-read-only (calculate-approval-progress (proposal-id uint))
    (match (map-get? governance-proposals proposal-id)
        proposal 
            (let ((token-id (get token-id proposal))
                  (multisig (map-get? multisig-config token-id)))
                (match multisig
                    config (ok {
                        current-approvals: (get approval-count proposal),
                        required-approvals: (get threshold config),
                        is-ready: (>= (get approval-count proposal) (get threshold config)),
                        expires-at: (get expires-at proposal)
                    })
                    (err err-nft-not-found)
                )
            )
        (err err-proposal-not-found)
    )
)

;; Update multi-sig threshold
(define-public (update-multisig-threshold (token-id uint) (new-threshold uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (multisig (unwrap! (map-get? multisig-config token-id) err-nft-not-found)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        (asserts! (> new-threshold u1) err-invalid-threshold)
        (asserts! (<= new-threshold (len (get co-signers multisig))) err-invalid-threshold)
        
        (map-set multisig-config token-id (merge multisig {
            threshold: new-threshold
        }))
        
        (ok new-threshold)
    )
)

;; Disable multi-sig governance
(define-public (disable-multisig-governance (token-id uint))
    (let ((token-owner (unwrap! (nft-get-owner? pension-nft token-id) err-nft-not-found))
          (multisig (unwrap! (map-get? multisig-config token-id) err-nft-not-found)))
        
        (asserts! (is-eq tx-sender token-owner) err-not-token-owner)
        
        (map-set multisig-config token-id (merge multisig {
            is-active: false
        }))
        
        (ok true)
    )
)


