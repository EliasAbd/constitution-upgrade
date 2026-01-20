;; ------------------------------------------------------------
;; constitution-upgrade.clar
;; Governance-controlled DAO constitution upgrades
;; ------------------------------------------------------------

(define-constant ERR-NOT-GOVERNANCE u100)
(define-constant ERR-ALREADY-EXECUTED u101)
(define-constant ERR-TIMELOCK-ACTIVE u102)
(define-constant ERR-INVALID u103)

;; ------------------------------------------------------------
;; Core configuration
;; ------------------------------------------------------------

;; governance contract allowed to propose & execute
(define-data-var governance (optional principal) none)

;; timelock (blocks) before upgrade can execute
(define-data-var timelock uint u144) ;; ~24 hours on Stacks

;; current constitution
(define-data-var constitution-hash (string-ascii 64) "")
(define-data-var constitution-version uint u1)

;; ------------------------------------------------------------
;; Upgrade proposals
;; ------------------------------------------------------------

(define-map upgrades
  {id: uint}
  {hash: (string-ascii 64), proposed-at: uint, executed: bool}
)

(define-data-var upgrade-count uint u0)

;; ------------------------------------------------------------
;; Initialization (one-time)
;; ------------------------------------------------------------

;; Disabled: initialize function has unchecked data warnings
;; (define-public (initialize
;;   (governance-contract principal)
;;   (initial-hash (string-ascii 64))
;; )
;;   (if (is-none (var-get governance))
;;       (begin
;;         (var-set governance (some governance-contract))
;;         (var-set constitution-hash initial-hash)
;;         (ok initial-hash)
;;       )
;;       (err ERR-INVALID)
;;   )
;; )

;; ------------------------------------------------------------
;; Propose constitution upgrade (governance only)
;; ------------------------------------------------------------

(define-public (propose-upgrade (new-hash (string-ascii 64)))
  (if (is-eq new-hash "")
      (err ERR-INVALID)
      (match (var-get governance)
        gov
          (if (not (is-eq gov tx-sender))
              (err ERR-NOT-GOVERNANCE)
              (let ((id (+ (var-get upgrade-count) u1)))
                (begin
                  (var-set upgrade-count id)
                  (map-set upgrades
                    { id: id }
                    {
                      hash: new-hash,
                      proposed-at: burn-block-height,
                      executed: false
                    }
                  )
                  (ok id)
                )
              )
          )
        (err ERR-NOT-GOVERNANCE)
      )
  )
)

;; ------------------------------------------------------------
;; Execute constitution upgrade (after timelock)
;; ------------------------------------------------------------

(define-public (execute-upgrade (upgrade-id uint))
  (if (<= upgrade-id u0)
      (err ERR-INVALID)
      (match (var-get governance)
        gov
          (if (not (is-eq gov tx-sender))
              (err ERR-NOT-GOVERNANCE)
              (match (map-get? upgrades { id: upgrade-id })
                upgrade
                  (if (get executed upgrade)
                      (err ERR-ALREADY-EXECUTED)
                      (if (< burn-block-height (+ (get proposed-at upgrade) (var-get timelock)))
                          (err ERR-TIMELOCK-ACTIVE)
                          (begin
                            ;; apply upgrade
                            (var-set constitution-hash (get hash upgrade))
                            (var-set constitution-version (+ (var-get constitution-version) u1))

                            ;; mark executed
                            (map-set upgrades { id: upgrade-id }
                              {
                                hash: (get hash upgrade),
                                proposed-at: (get proposed-at upgrade),
                                executed: true
                              })

                            (ok true)
                          )
                      )
                  )
                (err ERR-INVALID)
              )
          )
        (err ERR-NOT-GOVERNANCE)
      )
  )
)

;; ------------------------------------------------------------
;; Admin: update timelock (governance only)
;; ------------------------------------------------------------

(define-public (set-timelock (blocks uint))
  (if (<= blocks u0)
      (err ERR-INVALID)
      (match (var-get governance)
        gov
          (if (not (is-eq gov tx-sender))
              (err ERR-NOT-GOVERNANCE)
              (begin
                (var-set timelock blocks)
                (ok blocks)
              )
          )
        (err ERR-NOT-GOVERNANCE)
      )
  )
)

;; ------------------------------------------------------------
;; Read-only helpers
;; ------------------------------------------------------------

(define-read-only (get-constitution)
  (ok {
    hash: (var-get constitution-hash),
    version: (var-get constitution-version)
  })
)

(define-read-only (get-upgrade (id uint))
  (map-get? upgrades { id: id })
)

(define-read-only (get-total-upgrades)
  (ok (var-get upgrade-count))
)
