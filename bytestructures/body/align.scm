;;; align.scm --- Alignment calculation helpers.

;; Copyright © 2018 Taylan Kammer <taylan.kammer@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

;;; Either remains at 'position' or rounds up to the next multiple of
;;; 'alignment' depending on whether 'size' (if not greater than 'alignment')
;;; would fit.  Returns three values: the chosen position, the start of the
;;; alignment boundary of the chosen position, and the bit offset of the chosen
;;; position from the start of the alignment boundary.  A bit is represented by
;;; the value 1/8.
(define (align position size alignment)
  (let* ((integer (floor position))
         (fraction (- position integer)))
    (let-values (((prev-boundary-index offset) (floor/ integer alignment)))
      (let* ((prev-boundary (* prev-boundary-index alignment))
             (next-boundary (+ prev-boundary alignment)))
        (if (< next-boundary (+ position (min size alignment)))
            (values next-boundary next-boundary 0)
            (values position prev-boundary (* 8 (+ offset fraction))))))))

;;; Returns 'position' if it's already a multiple of 'alignment'; otherwise
;;; returns the next multiple.
(define (next-boundary position alignment)
  (align position +inf.0 alignment))

;;; align.scm ends here
