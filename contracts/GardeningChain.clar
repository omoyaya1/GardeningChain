;; GardeningChain: Organic Gardening and Cultivation Reward System
;; Version: 1.0.0

;; Constants
(define-constant GARDEN_CAPACITY u2000000)
(define-constant BASE_GARDENING_REWARD u26)
(define-constant ORGANIC_BONUS u10)
(define-constant MAX_GARDENER_LEVEL u14)
(define-constant ERR_INVALID_GARDENING_ACTIVITY u1)
(define-constant ERR_NO_GARDENING_TOKENS u2)
(define-constant ERR_GARDEN_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_GROWING_SEASON u1872)
(define-constant SEED_PRESERVATION_MULTIPLIER u5)
(define-constant MIN_PRESERVATION_PERIOD u936)
(define-constant EARLY_GARDENING_PENALTY u16)

;; Data Variables
(define-data-var total-gardening-tokens-distributed uint u0)
(define-data-var total-gardening-activities uint u0)
(define-data-var garden-supervisor principal tx-sender)

;; Data Maps
(define-map gardener-activities principal uint)
(define-map gardener-gardening-tokens principal uint)
(define-map gardening-activity-start-time principal uint)
(define-map gardener-organic-level principal uint)
(define-map gardener-last-activity principal uint)
(define-map gardener-preserved-seeds principal uint)
(define-map gardener-preservation-start-block principal uint)
(define-map crop-type-specialty principal uint)
(define-map gardener-harvest-count principal uint)
(define-map cultivation-mastery principal uint)

;; Public Functions
(define-public (start-planting-season (growing-period uint) (crop-type uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (and (> growing-period u0) (> crop-type u0) (<= crop-type u15)) (err ERR_INVALID_GARDENING_ACTIVITY))
    (map-set gardening-activity-start-time gardener burn-block-height)
    (map-set crop-type-specialty gardener crop-type)
    (ok true)
  ))

(define-public (complete-harvest (growing-period uint) (yield-quality uint))
  (let
    (
      (gardener tx-sender)
      (start-block (default-to u0 (map-get? gardening-activity-start-time gardener)))
      (blocks-growing (- burn-block-height start-block))
      (last-activity-block (default-to u0 (map-get? gardener-last-activity gardener)))
      (organic-level (default-to u0 (map-get? gardener-organic-level gardener)))
      (capped-organic (if (<= organic-level MAX_GARDENER_LEVEL) organic-level MAX_GARDENER_LEVEL))
      (cultivation-bonus (default-to u0 (map-get? cultivation-mastery gardener)))
      (yield-bonus (/ (* yield-quality u8) u100))
      (gardening-reward (+ BASE_GARDENING_REWARD (* capped-organic ORGANIC_BONUS) cultivation-bonus yield-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-growing growing-period) (<= yield-quality u100)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-activities gardener (+ (default-to u0 (map-get? gardener-activities gardener)) u1))
    (map-set gardener-gardening-tokens gardener (+ (default-to u0 (map-get? gardener-gardening-tokens gardener)) gardening-reward))
    
    (if (< (- burn-block-height last-activity-block) BLOCKS_PER_GROWING_SEASON)
      (map-set gardener-organic-level gardener (+ organic-level u1))
      (map-set gardener-organic-level gardener u1)
    )
    
    (if (>= yield-quality u80)
      (begin
        (map-set gardener-harvest-count gardener (+ (default-to u0 (map-get? gardener-harvest-count gardener)) u1))
        (map-set cultivation-mastery gardener (+ cultivation-bonus u4))
      )
      true
    )
    
    (map-set gardener-last-activity gardener burn-block-height)
    (var-set total-gardening-activities (+ (var-get total-gardening-activities) u1))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) gardening-reward))
    
    (asserts! (<= (var-get total-gardening-tokens-distributed) GARDEN_CAPACITY) (err ERR_GARDEN_CAPACITY_EXCEEDED))
    (ok gardening-reward)
  ))

(define-public (claim-gardening-rewards)
  (let
    (
      (gardener tx-sender)
      (token-balance (default-to u0 (map-get? gardener-gardening-tokens gardener)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_GARDENING_TOKENS))
    (map-set gardener-gardening-tokens gardener u0)
    (ok token-balance)
  ))

;; Seed Preservation Features
(define-public (preserve-seeds (amount uint))
  (let
    (
      (gardener tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_GARDENING_ACTIVITY))
    (asserts! (>= (var-get total-gardening-tokens-distributed) amount) (err ERR_GARDEN_CAPACITY_EXCEEDED))
    
    (map-set gardener-preserved-seeds gardener amount)
    (map-set gardener-preservation-start-block gardener burn-block-height)
    (var-set total-gardening-tokens-distributed (- (var-get total-gardening-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (release-preserved-seeds)
  (let
    (
      (gardener tx-sender)
      (preserved-amount (default-to u0 (map-get? gardener-preserved-seeds gardener)))
      (preservation-start-block (default-to u0 (map-get? gardener-preservation-start-block gardener)))
      (blocks-preserved (- burn-block-height preservation-start-block))
      (penalty (if (< blocks-preserved MIN_PRESERVATION_PERIOD) (/ (* preserved-amount EARLY_GARDENING_PENALTY) u100) u0))
      (preservation-bonus (if (>= blocks-preserved MIN_PRESERVATION_PERIOD) (/ (* preserved-amount SEED_PRESERVATION_MULTIPLIER) u100) u0))
      (final-amount (+ (- preserved-amount penalty) preservation-bonus))
    )
    (asserts! (> preserved-amount u0) (err ERR_NO_GARDENING_TOKENS))
    
    (map-set gardener-preserved-seeds gardener u0)
    (map-set gardener-preservation-start-block gardener u0)
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (establish-community-garden (garden-name (string-utf8 64)) (plot-count uint))
  (let
    (
      (gardener tx-sender)
      (organic-level (default-to u0 (map-get? gardener-organic-level gardener)))
      (harvest-count (default-to u0 (map-get? gardener-harvest-count gardener)))
      (community-bonus (+ (* plot-count u20) (* harvest-count u12) BASE_GARDENING_REWARD))
    )
    (asserts! (and (> (len garden-name) u0) (>= organic-level u8) (> plot-count u0)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-gardening-tokens gardener (+ (default-to u0 (map-get? gardener-gardening-tokens gardener)) community-bonus))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) community-bonus))
    
    (ok community-bonus)
  ))

(define-public (teach-gardening-workshop (student-count uint) (workshop-hours uint))
  (let
    (
      (gardener tx-sender)
      (organic-level (default-to u0 (map-get? gardener-organic-level gardener)))
      (cultivation-mastery-level (default-to u0 (map-get? cultivation-mastery gardener)))
      (teaching-bonus (+ (* student-count u18) (* workshop-hours u7) (* cultivation-mastery-level u2)))
    )
    (asserts! (and (> student-count u0) (> workshop-hours u0) (>= organic-level u10)) (err ERR_INVALID_GARDENING_ACTIVITY))
    
    (map-set gardener-gardening-tokens gardener (+ (default-to u0 (map-get? gardener-gardening-tokens gardener)) teaching-bonus))
    (var-set total-gardening-tokens-distributed (+ (var-get total-gardening-tokens-distributed) teaching-bonus))
    
    (ok teaching-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-gardening-activity-count (user principal))
  (default-to u0 (map-get? gardener-activities user)))

(define-read-only (get-gardening-token-balance (user principal))
  (default-to u0 (map-get? gardener-gardening-tokens user)))

(define-read-only (get-organic-level (user principal))
  (default-to u0 (map-get? gardener-organic-level user)))

(define-read-only (get-harvest-count (user principal))
  (default-to u0 (map-get? gardener-harvest-count user)))

(define-read-only (get-preserved-seeds (user principal))
  (default-to u0 (map-get? gardener-preserved-seeds user)))

(define-read-only (get-cultivation-mastery (user principal))
  (default-to u0 (map-get? cultivation-mastery user)))

(define-read-only (get-garden-stats)
  {
    total-gardening-activities: (var-get total-gardening-activities),
    total-gardening-tokens-distributed: (var-get total-gardening-tokens-distributed),
    garden-capacity: GARDEN_CAPACITY
  })

(define-read-only (calculate-gardening-reward (organic-level uint) (yield-quality uint) (cultivation-bonus uint))
  (let
    (
      (capped-organic (if (<= organic-level MAX_GARDENER_LEVEL) organic-level MAX_GARDENER_LEVEL))
      (yield-bonus (/ (* yield-quality u8) u100))
    )
    (+ BASE_GARDENING_REWARD (* capped-organic ORGANIC_BONUS) cultivation-bonus yield-bonus)
  ))

;; Private Functions
(define-private (is-garden-supervisor)
  (is-eq tx-sender (var-get garden-supervisor)))

(define-private (validate-gardening-parameters (growing-period uint) (yield-quality uint))
  (and (> growing-period u0) (<= yield-quality u100)))