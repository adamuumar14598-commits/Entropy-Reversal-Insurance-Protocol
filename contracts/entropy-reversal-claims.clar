;; title: entropy-reversal-claims
;; version: 1.0.0
;; summary: Automated payouts for thermodynamic violations and entropy reversal failures
;; description: Claims processing contract for entropy reversal insurance with automated assessment and payouts

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_OWNER_ONLY (err u300))
(define-constant ERR_INVALID_CLAIM (err u301))
(define-constant ERR_UNAUTHORIZED (err u302))
(define-constant ERR_INSUFFICIENT_FUNDS (err u303))
(define-constant ERR_CLAIM_EXPIRED (err u304))

(define-constant MAX_CLAIM_AMOUNT u10000000)
(define-constant MIN_CLAIM_AMOUNT u1000)
(define-constant CLAIM_EXPIRY_BLOCKS u1000)
(define-constant BASE_PREMIUM u500)
(define-constant MAX_POLICIES u100)

;; Claim severity levels
(define-constant SEVERITY_MINOR u1)
(define-constant SEVERITY_MODERATE u2)
(define-constant SEVERITY_MAJOR u3)
(define-constant SEVERITY_CRITICAL u4)
(define-constant SEVERITY_CATASTROPHIC u5)

;; Claim types
(define-constant TYPE_THERMODYNAMIC_VIOLATION u1)
(define-constant TYPE_ENTROPY_REVERSAL_FAILURE u2)
(define-constant TYPE_TIME_FLOW_DISRUPTION u3)
(define-constant TYPE_UNIVERSAL_DECAY_ACCELERATION u4)
(define-constant TYPE_EQUIPMENT_DAMAGE u5)

;; data vars
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-claims uint u0)
(define-data-var total-payouts uint u0)
(define-data-var reserve-funds uint u50000000)
(define-data-var processing-enabled bool true)
(define-data-var total-policies uint u0)

;; data maps
(define-map insurance-policies
  uint  ;; policy-id
  {
    policyholder: principal,
    policy-type: uint,
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    active: bool,
    claim-count: uint,
    deductible: uint
  }
)

(define-map claims
  uint  ;; claim-id
  {
    policy-id: uint,
    claimant: principal,
    claim-type: uint,
    severity: uint,
    incident-block: uint,
    reported-block: uint,
    amount-requested: uint,
    amount-approved: uint,
    status: (string-ascii 20)
  }
)

(define-map payout-records
  uint  ;; payout-id
  {
    claim-id: uint,
    recipient: principal,
    amount: uint,
    payout-block: uint
  }
)

;; public functions

(define-public (create-policy (policy-type uint) (coverage-amount uint) (duration-blocks uint))
  (let (
    (policy-id (+ (var-get total-policies) u1))
    (premium (calculate-premium policy-type coverage-amount))
    (current-block u3000)
  )
    (asserts! (<= policy-id MAX_POLICIES) ERR_INVALID_CLAIM)
    (asserts! (and (<= coverage-amount MAX_CLAIM_AMOUNT) (>= coverage-amount MIN_CLAIM_AMOUNT)) ERR_INVALID_CLAIM)
    (asserts! (> duration-blocks u0) ERR_INVALID_CLAIM)
    
    (map-set insurance-policies policy-id {
      policyholder: tx-sender,
      policy-type: policy-type,
      coverage-amount: coverage-amount,
      premium-paid: premium,
      start-block: current-block,
      end-block: (+ current-block duration-blocks),
      active: true,
      claim-count: u0,
      deductible: (/ coverage-amount u100)
    })
    
    (var-set total-policies policy-id)
    (var-set reserve-funds (+ (var-get reserve-funds) premium))
    
    (ok policy-id)
  )
)

(define-public (submit-claim (policy-id uint) (claim-type uint) (severity uint) 
                           (incident-block uint) (amount-requested uint))
  (let (
    (claim-id (+ (var-get total-claims) u1))
    (policy-data (map-get? insurance-policies policy-id))
    (current-block u3000)
  )
    (asserts! (is-some policy-data) ERR_INVALID_CLAIM)
    (asserts! (is-eq tx-sender (get policyholder (unwrap-panic policy-data))) ERR_UNAUTHORIZED)
    (asserts! (get active (unwrap-panic policy-data)) ERR_INVALID_CLAIM)
    (asserts! (policy-valid-at-incident policy-id incident-block) ERR_CLAIM_EXPIRED)
    (asserts! (and (<= amount-requested (get coverage-amount (unwrap-panic policy-data))) 
                   (>= amount-requested MIN_CLAIM_AMOUNT)) ERR_INVALID_CLAIM)
    (asserts! (and (>= severity SEVERITY_MINOR) (<= severity SEVERITY_CATASTROPHIC)) ERR_INVALID_CLAIM)
    (asserts! (and (>= claim-type TYPE_THERMODYNAMIC_VIOLATION) (<= claim-type TYPE_EQUIPMENT_DAMAGE)) ERR_INVALID_CLAIM)

    (map-set claims claim-id {
      policy-id: policy-id,
      claimant: tx-sender,
      claim-type: claim-type,
      severity: severity,
      incident-block: incident-block,
      reported-block: current-block,
      amount-requested: amount-requested,
      amount-approved: u0,
      status: "SUBMITTED"
    })

    (var-set total-claims claim-id)
    (ok claim-id)
  )
)

(define-public (process-claim (claim-id uint) (approved-amount uint) (decision (string-ascii 20)))
  (let (
    (claim-data (map-get? claims claim-id))
    (current-block u3000)
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_OWNER_ONLY)
    (asserts! (is-some claim-data) ERR_INVALID_CLAIM)
    (asserts! (<= approved-amount (get amount-requested (unwrap-panic claim-data))) ERR_INVALID_CLAIM)

    (map-set claims claim-id
             (merge (unwrap-panic claim-data) {
               amount-approved: approved-amount,
               status: decision
             }))

    (if (is-eq decision "APPROVED")
        (begin
          (try! (execute-payout claim-id approved-amount current-block))
          true
        )
        true
    )

    (ok true)
  )
)

;; read only

(define-read-only (get-policy (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-claim (claim-id uint))
  (map-get? claims claim-id)
)

(define-read-only (get-payout-record (payout-id uint))
  (map-get? payout-records payout-id)
)

;; private functions

(define-private (calculate-premium (policy-type uint) (coverage-amount uint))
  (let (
    (base-rate (type-to-base-rate policy-type))
    (coverage-factor (/ coverage-amount u10000))
  )
    (+ BASE_PREMIUM (* base-rate coverage-factor))
  )
)

(define-private (type-to-base-rate (policy-type uint))
  (if (is-eq policy-type TYPE_THERMODYNAMIC_VIOLATION)
      u150
      (if (is-eq policy-type TYPE_ENTROPY_REVERSAL_FAILURE)
          u200
          (if (is-eq policy-type TYPE_TIME_FLOW_DISRUPTION)
              u300
              (if (is-eq policy-type TYPE_UNIVERSAL_DECAY_ACCELERATION)
                  u400
                  u100
              )
          )
      )
  )
)

(define-private (policy-valid-at-incident (policy-id uint) (incident-block uint))
  (let (
    (policy-data (map-get? insurance-policies policy-id))
  )
    (match policy-data
      policy (and 
        (>= incident-block (get start-block policy))
        (<= incident-block (get end-block policy))
        (get active policy)
      )
      false
    )
  )
)

(define-private (execute-payout (claim-id uint) (amount uint) (current-block uint))
  (let (
    (payout-id (+ (get-next-payout-id) u1))
    (claim-data (unwrap-panic (map-get? claims claim-id)))
    (current-reserves (var-get reserve-funds))
  )
    (asserts! (>= current-reserves amount) ERR_INSUFFICIENT_FUNDS)

    (map-set payout-records payout-id {
      claim-id: claim-id,
      recipient: (get claimant claim-data),
      amount: amount,
      payout-block: current-block
    })

    (var-set reserve-funds (- current-reserves amount))
    (var-set total-payouts (+ (var-get total-payouts) amount))

    (ok payout-id)
  )
)

(define-private (get-next-payout-id)
  (fold max-payout-id (map-payout-ids) u0)
)

(define-private (max-payout-id (id uint) (acc uint))
  (if (> id acc) id acc)
)

(define-private (map-payout-ids)
  (list u0)
)
