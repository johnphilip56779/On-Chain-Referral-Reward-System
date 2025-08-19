(define-constant CONTRACT_OWNER tx-sender)

(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_USER_ALREADY_REGISTERED (err u1001))
(define-constant ERR_USER_NOT_FOUND (err u1002))
(define-constant ERR_INVALID_REFERRAL_CODE (err u1003))
(define-constant ERR_CANNOT_REFER_SELF (err u1004))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1005))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u1006))
(define-constant ERR_NO_REWARDS_AVAILABLE (err u1007))

(define-data-var next-referral-code uint u1000000)
(define-data-var total-users uint u0)
(define-data-var total-referrals uint u0)
(define-data-var reward-per-referral uint u1000000)
(define-data-var contract-balance uint u0)

(define-map users
  principal
  {
    referral-code: uint,
    referred-by: (optional principal),
    total-referrals: uint,
    total-rewards: uint,
    is-active: bool
  }
)

(define-map referral-codes
  uint
  principal
)

(define-map referral-rewards
  principal
  {
    available-rewards: uint,
    claimed-rewards: uint
  }
)

(define-map referral-history
  { referrer: principal, referee: principal }
  {
    timestamp: uint,
    reward-amount: uint,
    is-claimed: bool
  }
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)

(define-read-only (get-user-info (user principal))
  (map-get? users user)
)

(define-read-only (get-user-by-referral-code (code uint))
  (map-get? referral-codes code)
)

(define-read-only (get-reward-info (user principal))
  (map-get? referral-rewards user)
)

(define-read-only (get-total-stats)
  {
    total-users: (var-get total-users),
    total-referrals: (var-get total-referrals),
    reward-per-referral: (var-get reward-per-referral),
    contract-balance: (var-get contract-balance)
  }
)

(define-read-only (get-referral-history (referrer principal) (referee principal))
  (map-get? referral-history { referrer: referrer, referee: referee })
)

(define-public (register-user (referred-by-code (optional uint)))
  (let
    (
      (user-exists (map-get? users tx-sender))
      (new-code (var-get next-referral-code))
    )
    (asserts! (is-none user-exists) ERR_USER_ALREADY_REGISTERED)
    
    (match referred-by-code
      code
      (let
        (
          (referrer (map-get? referral-codes code))
        )
        (match referrer
          ref-user
          (begin
            (asserts! (not (is-eq ref-user tx-sender)) ERR_CANNOT_REFER_SELF)
            (map-set users tx-sender
              {
                referral-code: new-code,
                referred-by: (some ref-user),
                total-referrals: u0,
                total-rewards: u0,
                is-active: true
              }
            )
            (map-set referral-codes new-code tx-sender)
            (map-set referral-rewards tx-sender
              {
                available-rewards: u0,
                claimed-rewards: u0
              }
            )
            (try! (process-referral ref-user tx-sender))
            (var-set next-referral-code (+ new-code u1))
            (var-set total-users (+ (var-get total-users) u1))
            (ok new-code)
          )
          ERR_INVALID_REFERRAL_CODE
        )
      )
      (begin
        (map-set users tx-sender
          {
            referral-code: new-code,
            referred-by: none,
            total-referrals: u0,
            total-rewards: u0,
            is-active: true
          }
        )
        (map-set referral-codes new-code tx-sender)
        (map-set referral-rewards tx-sender
          {
            available-rewards: u0,
            claimed-rewards: u0
          }
        )
        (var-set next-referral-code (+ new-code u1))
        (var-set total-users (+ (var-get total-users) u1))
        (ok new-code)
      )
    )
  )
)

(define-private (process-referral (referrer principal) (referee principal))
  (let
    (
      (referrer-data (unwrap! (map-get? users referrer) ERR_USER_NOT_FOUND))
      (reward-amount (var-get reward-per-referral))
      (current-rewards (default-to { available-rewards: u0, claimed-rewards: u0 } (map-get? referral-rewards referrer)))
    )
    (map-set users referrer
      (merge referrer-data { total-referrals: (+ (get total-referrals referrer-data) u1) })
    )
    (map-set referral-rewards referrer
      {
        available-rewards: (+ (get available-rewards current-rewards) reward-amount),
        claimed-rewards: (get claimed-rewards current-rewards)
      }
    )
    (map-set referral-history { referrer: referrer, referee: referee }
      {
        timestamp: stacks-block-height,
        reward-amount: reward-amount,
        is-claimed: false
      }
    )
    (var-set total-referrals (+ (var-get total-referrals) u1))
    (ok true)
  )
)

(define-public (claim-rewards)
  (let
    (
      (user-data (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND))
      (reward-data (unwrap! (map-get? referral-rewards tx-sender) ERR_NO_REWARDS_AVAILABLE))
      (available (get available-rewards reward-data))
      (current-balance (var-get contract-balance))
    )
    (asserts! (> available u0) ERR_NO_REWARDS_AVAILABLE)
    (asserts! (>= current-balance available) ERR_INSUFFICIENT_BALANCE)
    
    (map-set referral-rewards tx-sender
      {
        available-rewards: u0,
        claimed-rewards: (+ (get claimed-rewards reward-data) available)
      }
    )
    (map-set users tx-sender
      (merge user-data { total-rewards: (+ (get total-rewards user-data) available) })
    )
    (var-set contract-balance (- current-balance available))
    (as-contract (stx-transfer? available tx-sender tx-sender))
  )
)

(define-public (fund-contract (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

(define-public (set-reward-amount (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set reward-per-referral new-amount)
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (current-balance (var-get contract-balance))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
    (var-set contract-balance (- current-balance amount))
    (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER))
  )
)

(define-public (deactivate-user (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set users user
      (merge user-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (reactivate-user (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set users user
      (merge user-data { is-active: true })
    )
    (ok true)
  )
)

(define-read-only (get-user-referrals (user principal))
  (let
    (
      (user-data (map-get? users user))
    )
    (match user-data
      data (ok (get total-referrals data))
      ERR_USER_NOT_FOUND
    )
  )
)

(define-read-only (check-user-active (user principal))
  (let
    (
      (user-data (map-get? users user))
    )
    (match user-data
      data (ok (get is-active data))
      ERR_USER_NOT_FOUND
    )
  )
)
