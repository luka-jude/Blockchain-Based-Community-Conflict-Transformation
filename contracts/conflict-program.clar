;; Community Conflict Transformation Platform
;; A blockchain-based system for addressing systemic community conflicts
;; through dialogue facilitation, mediation support, and reconciliation tracking

;; ===================================
;; CONSTANTS AND ERRORS
;; ===================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_PARTICIPANT (err u104))
(define-constant ERR_PROCESS_COMPLETE (err u105))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u106))
(define-constant ERR_INVALID_RATING (err u107))

;; Process Status Constants
(define-constant STATUS_INITIATED u1)
(define-constant STATUS_DIALOGUE u2)
(define-constant STATUS_MEDIATION u3)
(define-constant STATUS_RECONCILIATION u4)
(define-constant STATUS_COMPLETED u5)
(define-constant STATUS_SUSPENDED u6)

;; Stakeholder Types
(define-constant STAKEHOLDER_COMMUNITY_MEMBER u1)
(define-constant STAKEHOLDER_MEDIATOR u2)
(define-constant STAKEHOLDER_FACILITATOR u3)
(define-constant STAKEHOLDER_OBSERVER u4)

;; ===================================
;; DATA STRUCTURES
;; ===================================

;; Conflict Resolution Process
(define-map conflict-processes
  { process-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    initiator: principal,
    status: uint,
    current-phase: uint,
    created-at: uint,
    updated-at: uint,
    completion-target: uint,
    social-cohesion-score: uint,
    participants-count: uint,
    is-active: bool
  }
)

;; Stakeholder Registry
(define-map stakeholders
  { stakeholder-id: principal }
  {
    name: (string-ascii 50),
    stakeholder-type: uint,
    reputation-score: uint,
    processes-participated: uint,
    successful-resolutions: uint,
    certified-mediator: bool,
    registration-date: uint,
    is-active: bool
  }
)

;; Process Participation
(define-map process-participants
  { process-id: uint, participant: principal }
  {
    role: uint,
    joined-at: uint,
    commitment-level: uint,
    contribution-score: uint,
    satisfaction-rating: uint,
    is-active: bool
  }
)

;; Dialogue Sessions
(define-map dialogue-sessions
  { process-id: uint, session-id: uint }
  {
    facilitator: principal,
    session-date: uint,
    duration-minutes: uint,
    participants-present: uint,
    breakthrough-achieved: bool,
    consensus-level: uint,
    next-steps: (string-ascii 200),
    session-notes-hash: (string-ascii 64)
  }
)

;; Mediation Records
(define-map mediation-records
  { process-id: uint, mediation-id: uint }
  {
    mediator: principal,
    mediation-type: (string-ascii 30),
    start-date: uint,
    resolution-proposal: (string-ascii 300),
    agreement-reached: bool,
    implementation-timeline: uint,
    follow-up-required: bool,
    success-metrics: (string-ascii 200)
  }
)

;; Reconciliation Tracking
(define-map reconciliation-progress
  { process-id: uint }
  {
    healing-indicators: uint,
    trust-rebuilding-score: uint,
    community-acceptance: uint,
    behavioral-changes: uint,
    relationship-restoration: uint,
    long-term-sustainability: uint,
    last-assessment: uint,
    assessor: principal
  }
)

;; Social Cohesion Metrics
(define-map cohesion-metrics
  { community-id: (string-ascii 50), measurement-period: uint }
  {
    cooperation-index: uint,
    trust-level: uint,
    collective-efficacy: uint,
    social-capital: uint,
    conflict-frequency: uint,
    resolution-success-rate: uint,
    community-wellbeing: uint,
    measured-by: principal,
    measurement-date: uint
  }
)

;; ===================================
;; COUNTERS AND STATE
;; ===================================

(define-data-var next-process-id uint u1)
(define-data-var next-session-id uint u1)
(define-data-var next-mediation-id uint u1)
(define-data-var total-active-processes uint u0)
(define-data-var total-completed-processes uint u0)
(define-data-var platform-reputation-threshold uint u50)

;; ===================================
;; STAKEHOLDER MANAGEMENT
;; ===================================

;; Register as a stakeholder in the conflict transformation platform
(define-public (register-stakeholder (name (string-ascii 50)) (stakeholder-type uint))
  (let ((stakeholder-data {
    name: name,
    stakeholder-type: stakeholder-type,
    reputation-score: u0,
    processes-participated: u0,
    successful-resolutions: u0,
    certified-mediator: false,
    registration-date: stacks-block-height,
    is-active: true
  }))
  (if (is-some (map-get? stakeholders { stakeholder-id: tx-sender }))
    ERR_ALREADY_EXISTS
    (ok (map-set stakeholders { stakeholder-id: tx-sender } stakeholder-data))
  ))
)

;; Update stakeholder reputation based on performance
(define-public (update-reputation (stakeholder principal) (score-change int))
  (let ((stakeholder-info (unwrap! (map-get? stakeholders { stakeholder-id: stakeholder }) ERR_NOT_FOUND)))
    (if (is-eq tx-sender CONTRACT_OWNER)
      (let ((new-score (+ (get reputation-score stakeholder-info) (if (> score-change 0) (to-uint score-change) u0))))
        (ok (map-set stakeholders
          { stakeholder-id: stakeholder }
          (merge stakeholder-info { reputation-score: new-score })))
      )
      ERR_UNAUTHORIZED
    )
  )
)

;; Certify a mediator
(define-public (certify-mediator (mediator principal))
  (let ((stakeholder-info (unwrap! (map-get? stakeholders { stakeholder-id: mediator }) ERR_NOT_FOUND)))
    (if (and (is-eq tx-sender CONTRACT_OWNER)
             (>= (get reputation-score stakeholder-info) (var-get platform-reputation-threshold)))
      (ok (map-set stakeholders
        { stakeholder-id: mediator }
        (merge stakeholder-info { certified-mediator: true })))
      ERR_UNAUTHORIZED
    )
  )
)

;; ===================================
;; CONFLICT PROCESS MANAGEMENT
;; ===================================

;; Initiate a new conflict resolution process
(define-public (initiate-process
  (title (string-ascii 100))
  (description (string-ascii 500))
  (completion-target uint))
  (let ((process-id (var-get next-process-id))
        (process-data {
          title: title,
          description: description,
          initiator: tx-sender,
          status: STATUS_INITIATED,
          current-phase: u1,
          created-at: stacks-block-height,
          updated-at: stacks-block-height,
          completion-target: completion-target,
          social-cohesion-score: u0,
          participants-count: u1,
          is-active: true
        }))
    (map-set conflict-processes { process-id: process-id } process-data)
    (map-set process-participants
      { process-id: process-id, participant: tx-sender }
      {
        role: STAKEHOLDER_COMMUNITY_MEMBER,
        joined-at: stacks-block-height,
        commitment-level: u100,
        contribution-score: u0,
        satisfaction-rating: u0,
        is-active: true
      })
    (var-set next-process-id (+ process-id u1))
    (var-set total-active-processes (+ (var-get total-active-processes) u1))
    (ok process-id)
  )
)

;; Join an existing conflict resolution process
(define-public (join-process (process-id uint) (role uint) (commitment-level uint))
  (let ((process-info (unwrap! (map-get? conflict-processes { process-id: process-id }) ERR_NOT_FOUND))
        (stakeholder-info (unwrap! (map-get? stakeholders { stakeholder-id: tx-sender }) ERR_INVALID_PARTICIPANT)))
    (if (and (get is-active process-info)
             (< (get status process-info) STATUS_COMPLETED)
             (is-none (map-get? process-participants { process-id: process-id, participant: tx-sender })))
      (begin
        (map-set process-participants
          { process-id: process-id, participant: tx-sender }
          {
            role: role,
            joined-at: stacks-block-height,
            commitment-level: commitment-level,
            contribution-score: u0,
            satisfaction-rating: u0,
            is-active: true
          })
        (map-set conflict-processes
          { process-id: process-id }
          (merge process-info {
            participants-count: (+ (get participants-count process-info) u1),
            updated-at: stacks-block-height
          }))
        (ok true)
      )
      ERR_INVALID_STATUS
    )
  )
)

;; Update process status and phase
(define-public (update-process-status (process-id uint) (new-status uint) (new-phase uint))
  (let ((process-info (unwrap! (map-get? conflict-processes { process-id: process-id }) ERR_NOT_FOUND))
        (participant-info (unwrap! (map-get? process-participants { process-id: process-id, participant: tx-sender }) ERR_UNAUTHORIZED)))
    (if (or (is-eq (get initiator process-info) tx-sender)
            (is-eq (get role participant-info) STAKEHOLDER_FACILITATOR))
      (begin
        (map-set conflict-processes
          { process-id: process-id }
          (merge process-info {
            status: new-status,
            current-phase: new-phase,
            updated-at: stacks-block-height
          }))
        (if (is-eq new-status STATUS_COMPLETED)
          (begin
            (var-set total-completed-processes (+ (var-get total-completed-processes) u1))
            (var-set total-active-processes (- (var-get total-active-processes) u1))
          )
          true
        )
        (ok true)
      )
      ERR_UNAUTHORIZED
    )
  )
)

;; ===================================
;; DIALOGUE FACILITATION
;; ===================================

;; Record a dialogue session
(define-public (record-dialogue-session
  (process-id uint)
  (duration-minutes uint)
  (participants-present uint)
  (breakthrough-achieved bool)
  (consensus-level uint)
  (next-steps (string-ascii 200))
  (session-notes-hash (string-ascii 64)))
  (let ((session-id (var-get next-session-id))
        (process-info (unwrap! (map-get? conflict-processes { process-id: process-id }) ERR_NOT_FOUND))
        (participant-info (unwrap! (map-get? process-participants { process-id: process-id, participant: tx-sender }) ERR_UNAUTHORIZED)))
    (if (or (is-eq (get role participant-info) STAKEHOLDER_FACILITATOR)
            (is-eq (get role participant-info) STAKEHOLDER_MEDIATOR))
      (begin
        (map-set dialogue-sessions
          { process-id: process-id, session-id: session-id }
          {
            facilitator: tx-sender,
            session-date: stacks-block-height,
            duration-minutes: duration-minutes,
            participants-present: participants-present,
            breakthrough-achieved: breakthrough-achieved,
            consensus-level: consensus-level,
            next-steps: next-steps,
            session-notes-hash: session-notes-hash
          })
        (var-set next-session-id (+ session-id u1))
        (ok session-id)
      )
      ERR_UNAUTHORIZED
    )
  )
)

;; ===================================
;; MEDIATION SUPPORT
;; ===================================

;; Record mediation session
(define-public (record-mediation
  (process-id uint)
  (mediation-type (string-ascii 30))
  (resolution-proposal (string-ascii 300))
  (agreement-reached bool)
  (implementation-timeline uint)
  (follow-up-required bool)
  (success-metrics (string-ascii 200)))
  (let ((mediation-id (var-get next-mediation-id))
        (process-info (unwrap! (map-get? conflict-processes { process-id: process-id }) ERR_NOT_FOUND))
        (stakeholder-info (unwrap! (map-get? stakeholders { stakeholder-id: tx-sender }) ERR_UNAUTHORIZED)))
    (if (get certified-mediator stakeholder-info)
      (begin
        (map-set mediation-records
          { process-id: process-id, mediation-id: mediation-id }
          {
            mediator: tx-sender,
            mediation-type: mediation-type,
            start-date: stacks-block-height,
            resolution-proposal: resolution-proposal,
            agreement-reached: agreement-reached,
            implementation-timeline: implementation-timeline,
            follow-up-required: follow-up-required,
            success-metrics: success-metrics
          })
        (var-set next-mediation-id (+ mediation-id u1))
        (ok mediation-id)
      )
      ERR_INSUFFICIENT_REPUTATION
    )
  )
)

;; ===================================
;; RECONCILIATION TRACKING
;; ===================================

;; Update reconciliation progress
(define-public (update-reconciliation-progress
  (process-id uint)
  (healing-indicators uint)
  (trust-rebuilding-score uint)
  (community-acceptance uint)
  (behavioral-changes uint)
  (relationship-restoration uint)
  (long-term-sustainability uint))
  (let ((process-info (unwrap! (map-get? conflict-processes { process-id: process-id }) ERR_NOT_FOUND))
        (participant-info (unwrap! (map-get? process-participants { process-id: process-id, participant: tx-sender }) ERR_UNAUTHORIZED)))
    (if (or (is-eq (get initiator process-info) tx-sender)
            (>= (get role participant-info) STAKEHOLDER_MEDIATOR))
      (begin
        (map-set reconciliation-progress
          { process-id: process-id }
          {
            healing-indicators: healing-indicators,
            trust-rebuilding-score: trust-rebuilding-score,
            community-acceptance: community-acceptance,
            behavioral-changes: behavioral-changes,
            relationship-restoration: relationship-restoration,
            long-term-sustainability: long-term-sustainability,
            last-assessment: stacks-block-height,
            assessor: tx-sender
          })
        (ok true)
      )
      ERR_UNAUTHORIZED
    )
  )
)

;; ===================================
;; SOCIAL COHESION MEASUREMENT
;; ===================================

;; Record social cohesion metrics
(define-public (record-cohesion-metrics
  (community-id (string-ascii 50))
  (cooperation-index uint)
  (trust-level uint)
  (collective-efficacy uint)
  (social-capital uint)
  (conflict-frequency uint)
  (resolution-success-rate uint)
  (community-wellbeing uint))
  (let ((measurement-period stacks-block-height)
        (stakeholder-info (unwrap! (map-get? stakeholders { stakeholder-id: tx-sender }) ERR_UNAUTHORIZED)))
    (if (>= (get reputation-score stakeholder-info) (var-get platform-reputation-threshold))
      (begin
        (map-set cohesion-metrics
          { community-id: community-id, measurement-period: measurement-period }
          {
            cooperation-index: cooperation-index,
            trust-level: trust-level,
            collective-efficacy: collective-efficacy,
            social-capital: social-capital,
            conflict-frequency: conflict-frequency,
            resolution-success-rate: resolution-success-rate,
            community-wellbeing: community-wellbeing,
            measured-by: tx-sender,
            measurement-date: stacks-block-height
          })
        (ok measurement-period)
      )
      ERR_INSUFFICIENT_REPUTATION
    )
  )
)

;; ===================================
;; RATING AND FEEDBACK
;; ===================================

;; Rate process satisfaction
(define-public (rate-process-satisfaction (process-id uint) (satisfaction-rating uint))
  (let ((participant-info (unwrap! (map-get? process-participants { process-id: process-id, participant: tx-sender }) ERR_UNAUTHORIZED)))
    (if (and (<= satisfaction-rating u100) (> satisfaction-rating u0))
      (begin
        (map-set process-participants
          { process-id: process-id, participant: tx-sender }
          (merge participant-info { satisfaction-rating: satisfaction-rating }))
        (ok true)
      )
      ERR_INVALID_RATING
    )
  )
)

;; ===================================
;; READ-ONLY FUNCTIONS
;; ===================================

;; Get process information
(define-read-only (get-process-info (process-id uint))
  (map-get? conflict-processes { process-id: process-id })
)

;; Get stakeholder information
(define-read-only (get-stakeholder-info (stakeholder principal))
  (map-get? stakeholders { stakeholder-id: stakeholder })
)

;; Get process participant information
(define-read-only (get-participant-info (process-id uint) (participant principal))
  (map-get? process-participants { process-id: process-id, participant: participant })
)

;; Get dialogue session information
(define-read-only (get-dialogue-session (process-id uint) (session-id uint))
  (map-get? dialogue-sessions { process-id: process-id, session-id: session-id })
)

;; Get mediation record
(define-read-only (get-mediation-record (process-id uint) (mediation-id uint))
  (map-get? mediation-records { process-id: process-id, mediation-id: mediation-id })
)

;; Get reconciliation progress
(define-read-only (get-reconciliation-progress (process-id uint))
  (map-get? reconciliation-progress { process-id: process-id })
)

;; Get social cohesion metrics
(define-read-only (get-cohesion-metrics (community-id (string-ascii 50)) (measurement-period uint))
  (map-get? cohesion-metrics { community-id: community-id, measurement-period: measurement-period })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  (ok {
    total-active-processes: (var-get total-active-processes),
    total-completed-processes: (var-get total-completed-processes),
    next-process-id: (var-get next-process-id),
    reputation-threshold: (var-get platform-reputation-threshold)
  })
)
