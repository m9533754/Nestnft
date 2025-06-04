
;; title: Nestnft
;; version:
;; summary:
;; description:


;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

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

(define-data-var last-token-id uint u0)
(define-data-var total-pension-value uint u0)

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