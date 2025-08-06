;; Digital Garden Share Network - Plot Management Contract
;; Handles plot registration, booking, and basic management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-plot-occupied (err u103))
(define-constant err-plot-available (err u104))
(define-constant err-invalid-size (err u105))
(define-constant err-invalid-duration (err u106))

;; Data Variables
(define-data-var next-plot-id uint u1)
(define-data-var contract-active bool true)

;; Plot status enumeration
(define-constant plot-available u0)
(define-constant plot-booked u1)
(define-constant plot-active u2)
(define-constant plot-harvesting u3)

;; Data Maps
(define-map plots
  { plot-id: uint }
  {
    location: (string-ascii 100),
    size-sqft: uint,
    soil-type: (string-ascii 50),
    sunlight-hours: uint,
    water-access: bool,
    created-at: uint,
    owner: principal,
    status: uint
  }
)

(define-map plot-bookings
  { plot-id: uint }
  {
    gardener: principal,
    booked-at: uint,
    season-start: uint,
    season-end: uint,
    deposit-paid: uint,
    notes: (string-ascii 500)
  }
)

(define-map gardener-profiles
  { gardener: principal }
  {
    name: (string-ascii 100),
    experience-level: uint, ;; 1-5 scale
    preferred-crops: (string-ascii 200),
    contact-info: (string-ascii 150),
    total-seasons: uint,
    reputation-score: uint,
    joined-at: uint
  }
)

(define-map plot-owners
  { owner: principal }
  { total-plots: uint, active-plots: uint }
)

;; Read-only functions
(define-read-only (get-plot (plot-id uint))
  (map-get? plots { plot-id: plot-id })
)

(define-read-only (get-plot-booking (plot-id uint))
  (map-get? plot-bookings { plot-id: plot-id })
)

(define-read-only (get-gardener-profile (gardener principal))
  (map-get? gardener-profiles { gardener: gardener })
)

(define-read-only (get-plot-owner-stats (owner principal))
  (map-get? plot-owners { owner: owner })
)

(define-read-only (get-next-plot-id)
  (var-get next-plot-id)
)

(define-read-only (is-plot-available (plot-id uint))
  (match (map-get? plots { plot-id: plot-id })
    plot-data (is-eq (get status plot-data) plot-available)
    false
  )
)

;; Administrative functions
(define-public (register-plot (location (string-ascii 100)) (size-sqft uint) (soil-type (string-ascii 50)) (sunlight-hours uint) (water-access bool))
  (let (
    (plot-id (var-get next-plot-id))
    (current-block u1)
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (> size-sqft u0) err-invalid-size)
    (asserts! (<= sunlight-hours u24) err-invalid-size)

    (map-set plots
      { plot-id: plot-id }
      {
        location: location,
        size-sqft: size-sqft,
        soil-type: soil-type,
        sunlight-hours: sunlight-hours,
        water-access: water-access,
        created-at: current-block,
        owner: tx-sender,
        status: plot-available
      }
    )

    ;; Update owner stats
    (match (map-get? plot-owners { owner: tx-sender })
      existing-stats
        (map-set plot-owners
          { owner: tx-sender }
          {
            total-plots: (+ (get total-plots existing-stats) u1),
            active-plots: (get active-plots existing-stats)
          }
        )
      (map-set plot-owners
        { owner: tx-sender }
        { total-plots: u1, active-plots: u0 }
      )
    )

    (var-set next-plot-id (+ plot-id u1))
    (ok plot-id)
  )
)

;; Gardener functions
(define-public (create-gardener-profile (name (string-ascii 100)) (experience-level uint) (preferred-crops (string-ascii 200)) (contact-info (string-ascii 150)))
  (let ((current-block u1))
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (<= experience-level u5) err-invalid-size)
    (asserts! (> experience-level u0) err-invalid-size)

    (map-set gardener-profiles
      { gardener: tx-sender }
      {
        name: name,
        experience-level: experience-level,
        preferred-crops: preferred-crops,
        contact-info: contact-info,
        total-seasons: u0,
        reputation-score: u50, ;; Start with neutral score
        joined-at: current-block
      }
    )
    (ok true)
  )
)

(define-public (book-plot (plot-id uint) (season-duration-blocks uint) (notes (string-ascii 500)))
  (let (
    (plot-data (unwrap! (map-get? plots { plot-id: plot-id }) err-not-found))
    (current-block u1)
    (season-end (+ current-block season-duration-blocks))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (is-eq (get status plot-data) plot-available) err-plot-occupied)
    (asserts! (> season-duration-blocks u0) err-invalid-duration)
    (asserts! (<= season-duration-blocks u52560) err-invalid-duration) ;; Max ~1 year

    ;; Update plot status
    (map-set plots
      { plot-id: plot-id }
      (merge plot-data { status: plot-booked })
    )

    ;; Create booking record
    (map-set plot-bookings
      { plot-id: plot-id }
      {
        gardener: tx-sender,
        booked-at: current-block,
        season-start: current-block,
        season-end: season-end,
        deposit-paid: u0, ;; For future STX deposit implementation
        notes: notes
      }
    )

    ;; Update plot owner active count
    (match (map-get? plot-owners { owner: (get owner plot-data) })
      owner-stats
        (map-set plot-owners
          { owner: (get owner plot-data) }
          (merge owner-stats { active-plots: (+ (get active-plots owner-stats) u1) })
        )
      false ;; Should not happen if plot exists
    )

    (ok plot-id)
  )
)

(define-public (start-growing-season (plot-id uint))
  (let (
    (plot-data (unwrap! (map-get? plots { plot-id: plot-id }) err-not-found))
    (booking-data (unwrap! (map-get? plot-bookings { plot-id: plot-id }) err-not-found))
  )
    (asserts! (var-get contract-active) err-owner-only)
    (asserts! (is-eq (get gardener booking-data) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status plot-data) plot-booked) err-plot-available)
    (asserts! (>= u1 (get season-start booking-data)) err-unauthorized)

    ;; Update plot status to active growing
    (map-set plots
      { plot-id: plot-id }
      (merge plot-data { status: plot-active })
    )

    (ok true)
  )
)

(define-public (release-plot (plot-id uint))
  (let (
    (plot-data (unwrap! (map-get? plots { plot-id: plot-id }) err-not-found))
    (booking-data (unwrap! (map-get? plot-bookings { plot-id: plot-id }) err-not-found))
  )
    (asserts! (var-get contract-active) err-owner-only)

    ;; Allow release by plot owner, gardener, or after season end
    (asserts! (or
                (is-eq (get owner plot-data) tx-sender)
                (is-eq (get gardener booking-data) tx-sender)
                (> u1000000 (get season-end booking-data)))
              err-unauthorized)

    ;; Reset plot to available
    (map-set plots
      { plot-id: plot-id }
      (merge plot-data { status: plot-available })
    )

    ;; Remove booking
    (map-delete plot-bookings { plot-id: plot-id })

    ;; Update gardener stats
    (match (map-get? gardener-profiles { gardener: (get gardener booking-data) })
      gardener-data
        (map-set gardener-profiles
          { gardener: (get gardener booking-data) }
          (merge gardener-data { total-seasons: (+ (get total-seasons gardener-data) u1) })
        )
      false
    )

    ;; Update plot owner active count
    (match (map-get? plot-owners { owner: (get owner plot-data) })
      owner-stats
        (map-set plot-owners
          { owner: (get owner plot-data) }
          (merge owner-stats {
            active-plots: (if (> (get active-plots owner-stats) u0)
                            (- (get active-plots owner-stats) u1)
                            u0)
          })
        )
      false
    )

    (ok true)
  )
)

;; Contract management
(define-public (toggle-contract-active)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

(define-public (update-reputation (gardener principal) (new-score uint))
  (let ((gardener-data (unwrap! (map-get? gardener-profiles { gardener: gardener }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-score u100) err-invalid-size)

    (map-set gardener-profiles
      { gardener: gardener }
      (merge gardener-data { reputation-score: new-score })
    )
    (ok true)
  )
)
