(define-constant CONTRACT_OWNER tx-sender)

(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_USER_ALREADY_REGISTERED (err u1001))
(define-constant ERR_USER_NOT_FOUND (err u1002))
(define-constant ERR_INVALID_REFERRAL_CODE (err u1003))
(define-constant ERR_CANNOT_REFER_SELF (err u1004))
(define-constant ERR_INSUFFICIENT_BALANCE (err u1005))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u1006))
(define-constant ERR_NO_REWARDS_AVAILABLE (err u1007))
(define-constant ERR_INVALID_RANK (err u1008))
(define-constant ERR_MILESTONE_ALREADY_CLAIMED (err u1009))
(define-constant ERR_MILESTONE_NOT_REACHED (err u1010))
(define-constant ERR_INVALID_MILESTONE (err u1011))
(define-constant ERR_CONTRACT_PAUSED (err u1012))
(define-constant ERR_INVALID_PERCENTAGE (err u1014))

(define-data-var next-referral-code uint u1000000)
(define-data-var total-users uint u0)
(define-data-var total-referrals uint u0)
(define-data-var reward-per-referral uint u1000000)
(define-data-var contract-balance uint u0)
(define-data-var contract-paused bool false)
(define-data-var leaderboard-size uint u10)
(define-data-var milestone-1-threshold uint u5)
(define-data-var milestone-1-reward uint u5000000)
(define-data-var milestone-2-threshold uint u10)
(define-data-var milestone-2-reward uint u12000000)
(define-data-var milestone-3-threshold uint u25)
(define-data-var milestone-3-reward uint u35000000)
(define-data-var milestone-4-threshold uint u50)
(define-data-var milestone-4-reward uint u80000000)
(define-data-var milestone-5-threshold uint u100)
(define-data-var milestone-5-reward uint u200000000)

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

(define-map leaderboard-entries
  uint
  {
    user: principal,
    referral-count: uint
  }
)

(define-map milestone-claims
  { user: principal, milestone-id: uint }
  {
    claimed: bool,
    claim-timestamp: uint,
    reward-amount: uint
  }
)

(define-read-only (get-contract-owner)
  CONTRACT_OWNER
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
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

(define-private (process-referral (referrer principal) (referee principal))
  (let
    (
      (referrer-data (unwrap! (map-get? users referrer) ERR_USER_NOT_FOUND))
      (reward-amount (var-get reward-per-referral))
      (current-rewards (default-to { available-rewards: u0, claimed-rewards: u0 } (map-get? referral-rewards referrer)))
      (kickback-pct (default-to u0 (map-get? kickback-percentages referrer)))
      (kickback-amt (/ (* reward-amount kickback-pct) u100))
      (referrer-amt (- reward-amount kickback-amt))
      (referee-rewards (default-to { available-rewards: u0, claimed-rewards: u0 } (map-get? referral-rewards referee)))
    )
    (map-set users referrer
      (merge referrer-data { total-referrals: (+ (get total-referrals referrer-data) u1) })
    )
    (map-set referral-rewards referrer
      {
        available-rewards: (+ (get available-rewards current-rewards) referrer-amt),
        claimed-rewards: (get claimed-rewards current-rewards)
      }
    )
    (if (> kickback-amt u0)
      (map-set referral-rewards referee
        {
          available-rewards: (+ (get available-rewards referee-rewards) kickback-amt),
          claimed-rewards: (get claimed-rewards referee-rewards)
        }
      )
      true
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

(define-public (register-user (referred-by-code (optional uint)))
  (let
    (
      (user-exists (map-get? users tx-sender))
      (new-code (var-get next-referral-code))
    )
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
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

(define-public (claim-rewards)
  (let
    (
      (user-data (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND))
      (reward-data (unwrap! (map-get? referral-rewards tx-sender) ERR_NO_REWARDS_AVAILABLE))
      (available (get available-rewards reward-data))
      (current-balance (var-get contract-balance))
    )
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
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

(define-public (set-contract-pause (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set contract-paused paused)
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

(define-public (update-leaderboard-entry (user principal))
  (let
    (
      (user-data (unwrap! (map-get? users user) ERR_USER_NOT_FOUND))
      (referral-count (get total-referrals user-data))
    )
    (begin
      (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
      (update-leaderboard-simple user referral-count)
      (ok true)
    )
  )
)

(define-private (update-leaderboard-simple (user principal) (referral-count uint))
  (let
    (
      (entry-1 (map-get? leaderboard-entries u1))
      (entry-2 (map-get? leaderboard-entries u2))
      (entry-3 (map-get? leaderboard-entries u3))
      (entry-4 (map-get? leaderboard-entries u4))
      (entry-5 (map-get? leaderboard-entries u5))
    )
    (begin
      (if (or (is-none entry-1) (and (is-some entry-1) (> referral-count (get referral-count (unwrap-panic entry-1)))))
        (begin
          (if (is-some entry-4) (map-set leaderboard-entries u5 (unwrap-panic entry-4)) false)
          (if (is-some entry-3) (map-set leaderboard-entries u4 (unwrap-panic entry-3)) false)
          (if (is-some entry-2) (map-set leaderboard-entries u3 (unwrap-panic entry-2)) false)
          (if (is-some entry-1) (map-set leaderboard-entries u2 (unwrap-panic entry-1)) false)
          (map-set leaderboard-entries u1 {user: user, referral-count: referral-count})
          true
        )
        (if (or (is-none entry-2) (and (is-some entry-2) (> referral-count (get referral-count (unwrap-panic entry-2)))))
          (begin
            (if (is-some entry-4) (map-set leaderboard-entries u5 (unwrap-panic entry-4)) false)
            (if (is-some entry-3) (map-set leaderboard-entries u4 (unwrap-panic entry-3)) false)
            (if (is-some entry-2) (map-set leaderboard-entries u3 (unwrap-panic entry-2)) false)
            (map-set leaderboard-entries u2 {user: user, referral-count: referral-count})
            true
          )
          (if (> referral-count u0)
            (begin
              (map-set leaderboard-entries u5 {user: user, referral-count: referral-count})
              true
            )
            true
          )
        )
      )
      true
    )
  )
)

(define-read-only (get-leaderboard-entry (rank uint))
  (let
    (
      (board-size (var-get leaderboard-size))
    )
    (asserts! (<= rank board-size) ERR_INVALID_RANK)
    (asserts! (> rank u0) ERR_INVALID_RANK)
    (ok (map-get? leaderboard-entries rank))
  )
)

(define-read-only (get-top-referrers)
  (ok (list 
    (map-get? leaderboard-entries u1)
    (map-get? leaderboard-entries u2)
    (map-get? leaderboard-entries u3)
    (map-get? leaderboard-entries u4)
    (map-get? leaderboard-entries u5)
    (map-get? leaderboard-entries u6)
    (map-get? leaderboard-entries u7)
    (map-get? leaderboard-entries u8)
    (map-get? leaderboard-entries u9)
    (map-get? leaderboard-entries u10)
  ))
)

(define-read-only (find-user-rank (user principal))
  (let
    (
      (entry-1 (map-get? leaderboard-entries u1))
      (entry-2 (map-get? leaderboard-entries u2))
      (entry-3 (map-get? leaderboard-entries u3))
      (entry-4 (map-get? leaderboard-entries u4))
      (entry-5 (map-get? leaderboard-entries u5))
    )
    (if (and (is-some entry-1) (is-eq user (get user (unwrap-panic entry-1)))) (ok (some u1))
    (if (and (is-some entry-2) (is-eq user (get user (unwrap-panic entry-2)))) (ok (some u2))
    (if (and (is-some entry-3) (is-eq user (get user (unwrap-panic entry-3)))) (ok (some u3))
    (if (and (is-some entry-4) (is-eq user (get user (unwrap-panic entry-4)))) (ok (some u4))
    (if (and (is-some entry-5) (is-eq user (get user (unwrap-panic entry-5)))) (ok (some u5))
    (ok none))))))
  )
)

(define-private (get-milestone-threshold (milestone-id uint))
  (if (is-eq milestone-id u1) (var-get milestone-1-threshold)
  (if (is-eq milestone-id u2) (var-get milestone-2-threshold)
  (if (is-eq milestone-id u3) (var-get milestone-3-threshold)
  (if (is-eq milestone-id u4) (var-get milestone-4-threshold)
  (if (is-eq milestone-id u5) (var-get milestone-5-threshold)
  u0)))))
)

(define-private (get-milestone-reward (milestone-id uint))
  (if (is-eq milestone-id u1) (var-get milestone-1-reward)
  (if (is-eq milestone-id u2) (var-get milestone-2-reward)
  (if (is-eq milestone-id u3) (var-get milestone-3-reward)
  (if (is-eq milestone-id u4) (var-get milestone-4-reward)
  (if (is-eq milestone-id u5) (var-get milestone-5-reward)
  u0)))))
)

(define-read-only (get-milestone-status (user principal) (milestone-id uint))
  (let
    (
      (user-data (map-get? users user))
      (claim-data (map-get? milestone-claims { user: user, milestone-id: milestone-id }))
      (threshold (get-milestone-threshold milestone-id))
      (reward (get-milestone-reward milestone-id))
    )
    (match user-data
      data
      (ok {
        milestone-id: milestone-id,
        threshold: threshold,
        reward: reward,
        current-referrals: (get total-referrals data),
        is-reached: (>= (get total-referrals data) threshold),
        is-claimed: (match claim-data
          claim (get claimed claim)
          false
        )
      })
      ERR_USER_NOT_FOUND
    )
  )
)

(define-read-only (get-all-milestones (user principal))
  (let
    (
      (user-data (map-get? users user))
    )
    (match user-data
      data
      (ok (list
        (unwrap-panic (get-milestone-status user u1))
        (unwrap-panic (get-milestone-status user u2))
        (unwrap-panic (get-milestone-status user u3))
        (unwrap-panic (get-milestone-status user u4))
        (unwrap-panic (get-milestone-status user u5))
      ))
      ERR_USER_NOT_FOUND
    )
  )
)

(define-read-only (get-available-milestones (user principal))
  (let
    (
      (user-data (map-get? users user))
    )
    (match user-data
      data
      (let
        (
          (referrals (get total-referrals data))
          (m1-claim (map-get? milestone-claims {user: user, milestone-id: u1}))
          (m2-claim (map-get? milestone-claims {user: user, milestone-id: u2}))
          (m3-claim (map-get? milestone-claims {user: user, milestone-id: u3}))
          (m4-claim (map-get? milestone-claims {user: user, milestone-id: u4}))
          (m5-claim (map-get? milestone-claims {user: user, milestone-id: u5}))
          (m1-claimed (match m1-claim c (get claimed c) false))
          (m2-claimed (match m2-claim c (get claimed c) false))
          (m3-claimed (match m3-claim c (get claimed c) false))
          (m4-claimed (match m4-claim c (get claimed c) false))
          (m5-claimed (match m5-claim c (get claimed c) false))
        )
        (ok {
          milestone-1: (and (>= referrals (var-get milestone-1-threshold)) (not m1-claimed)),
          milestone-2: (and (>= referrals (var-get milestone-2-threshold)) (not m2-claimed)),
          milestone-3: (and (>= referrals (var-get milestone-3-threshold)) (not m3-claimed)),
          milestone-4: (and (>= referrals (var-get milestone-4-threshold)) (not m4-claimed)),
          milestone-5: (and (>= referrals (var-get milestone-5-threshold)) (not m5-claimed))
        })
      )
      ERR_USER_NOT_FOUND
    )
  )
)

(define-public (claim-milestone-reward (milestone-id uint))
  (let
    (
      (user-data (unwrap! (map-get? users tx-sender) ERR_USER_NOT_FOUND))
      (threshold (get-milestone-threshold milestone-id))
      (reward (get-milestone-reward milestone-id))
      (claim-key {user: tx-sender, milestone-id: milestone-id})
      (existing-claim (map-get? milestone-claims claim-key))
      (current-balance (var-get contract-balance))
    )
    (asserts! (not (var-get contract-paused)) ERR_CONTRACT_PAUSED)
    (asserts! (and (>= milestone-id u1) (<= milestone-id u5)) ERR_INVALID_MILESTONE)
    (asserts! (>= (get total-referrals user-data) threshold) ERR_MILESTONE_NOT_REACHED)
    (asserts! (is-none existing-claim) ERR_MILESTONE_ALREADY_CLAIMED)
    (asserts! (>= current-balance reward) ERR_INSUFFICIENT_BALANCE)
    
    (map-set milestone-claims claim-key
      {
        claimed: true,
        claim-timestamp: stacks-block-height,
        reward-amount: reward
      }
    )
    (map-set users tx-sender
      (merge user-data { total-rewards: (+ (get total-rewards user-data) reward) })
    )
    (var-set contract-balance (- current-balance reward))
    (as-contract (stx-transfer? reward tx-sender tx-sender))
  )
)

(define-public (set-milestone-config (milestone-id uint) (threshold uint) (reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (and (>= milestone-id u1) (<= milestone-id u5)) ERR_INVALID_MILESTONE)
    
    (if (is-eq milestone-id u1)
      (begin
        (var-set milestone-1-threshold threshold)
        (var-set milestone-1-reward reward)
        (ok true)
      )
      (if (is-eq milestone-id u2)
        (begin
          (var-set milestone-2-threshold threshold)
          (var-set milestone-2-reward reward)
          (ok true)
        )
        (if (is-eq milestone-id u3)
          (begin
            (var-set milestone-3-threshold threshold)
            (var-set milestone-3-reward reward)
            (ok true)
          )
          (if (is-eq milestone-id u4)
            (begin
              (var-set milestone-4-threshold threshold)
              (var-set milestone-4-reward reward)
              (ok true)
            )
            (if (is-eq milestone-id u5)
              (begin
                (var-set milestone-5-threshold threshold)
                (var-set milestone-5-reward reward)
                (ok true)
              )
              ERR_INVALID_MILESTONE
            )
          )
        )
      )
    )
  )
)

(define-read-only (get-milestone-config (milestone-id uint))
  (if (and (>= milestone-id u1) (<= milestone-id u5))
    (ok {
      milestone-id: milestone-id,
      threshold: (get-milestone-threshold milestone-id),
      reward: (get-milestone-reward milestone-id)
    })
    ERR_INVALID_MILESTONE
  )
)

(define-constant ERR_INVALID_MULTIPLIER (err u1013))

(define-data-var default-multiplier uint u100)

(define-map reward-multipliers
  principal
  uint
)

(define-read-only (get-reward-multiplier (user principal))
  (let
    (
      (fallback (var-get default-multiplier))
      (override (map-get? reward-multipliers user))
      (multiplier (default-to fallback override))
    )
    (ok multiplier)
  )
)

(define-public (set-reward-multiplier (user principal) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> multiplier u0) ERR_INVALID_MULTIPLIER)
    (map-set reward-multipliers user multiplier)
    (ok true)
  )
)

(define-public (set-default-multiplier (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> multiplier u0) ERR_INVALID_MULTIPLIER)
    (var-set default-multiplier multiplier)
    (ok true)
  )
)

(define-map kickback-percentages principal uint)

(define-read-only (get-kickback-percentage (user principal))
  (default-to u0 (map-get? kickback-percentages user))
)

(define-public (set-kickback-percentage (percentage uint))
  (begin
    (asserts! (<= percentage u100) ERR_INVALID_PERCENTAGE)
    (map-set kickback-percentages tx-sender percentage)
    (ok true)
  )
)
