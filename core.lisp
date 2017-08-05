(defpackage :punter/core
  (:use :cl :alexandria :usocket))

(in-package :punter/core)

(defparameter *online-game* nil)

(defstruct (state (:type list)
		  (:constructor make-state-1))
  mines
  rivers
  sites
  networks
  strategy-data
  my-id
  n-punters)

(defun main ()
  )

(defun make-state (num-sites)
  (make-state-1 :sites (make-array num-sites :initial-element nil)))

(defstruct (mine (:type list))
  site
  site-id
  mine-id)

(defstruct (site (:type list)
		 (:constructor make-site-1))
  id
  neighbors
  mine-paths)

(defun make-site (id num-mines)
  (let ((mine-paths (make-array num-mines)))
    (dotimes (i num-mines)
      (setf (aref mine-paths i) (make-mine-path-info)))
    (make-site-1 :id id :mine-paths mine-paths)))

(defstruct (mine-path-info (:type list))
  distance
  visited
  prev-site
  score)

(defstruct (network (:type list))
  site-ids
  sites
  mines)

(defun assocr (item alist)
  (cdr (assoc item alist)))

(defun river-id (source target)
  (let ((source-id (site-id source))
	(target-id (site-id target)))
    (if (< source-id target-id)
	(cons source-id target-id)
	(cons target-id source-id))))

(defun load-map (map)
  (let (state num-mines)
    (let ((max-site (loop as site in (assocr :sites map)
		       maximizing (assocr :id site)))
	  (num-sites (length (assocr :sites map))))
      (when (> max-site (* 2 num-sites))
	(error "Eto uz slishkom ~d vs ~d" max-site num-sites))
      (setf state (make-state (1+ max-site))))
    (setf num-mines (length (assocr :mines map)))
    (mapcar (lambda (site)
	      (let* ((id (assocr :id site))
		     (site (make-site id num-mines)))
		(setf (aref (state-sites state) id) site)))
	    (assocr :sites map))
    (mapcar (lambda (river)
	      (let* ((source (aref (state-sites state) (assocr :source river)))
		     (target (aref (state-sites state) (assocr :target river))))
		(push source (site-neighbors target))
		(push target (site-neighbors source))
		(push (river-id source target) (state-rivers state))))
	    (assocr :rivers map))
    (loop as mine in (assocr :mines map)
       as mine-id from 0
       do (push (make-mine :site (aref (state-sites state) mine)
			   :site-id mine
			   :mine-id mine-id)
		(state-mines state)))
    (setf (state-mines state) (nreverse (state-mines state)))
    (loop as mine in (state-mines state)
       as i from 0
       do (push (make-network :site-ids (list (mine-site-id mine))
			      :sites (list (mine-site mine))
			      :mines (list mine))
		(state-networks state))
       do (let ((mine-info (site-mine-path-info (mine-site mine) i)))
	    (setf (mine-path-info-distance mine-info) 0
		  (mine-path-info-visited mine-info) t))
       do (map-path state (list (mine-site mine)) i))
    (loop as site across (state-sites state)
       do (when site
	    (loop as mine-info across (site-mine-paths site)
	       do (setf (mine-path-info-score mine-info)
			(if-let ((distance (mine-path-info-distance mine-info)))
			  (* distance distance)
			  0)))))
    state))

(defun site-mine-path-info (site mine-id)
  (aref (site-mine-paths site) mine-id))

(defun map-path (state sites mine-id)
  (let (new-sources)
    (mapcar (lambda (source)
	      (let ((source-info (site-mine-path-info source mine-id)))
		(mapcar (lambda (target)
			  (let ((target-info (site-mine-path-info target mine-id)))
			    (unless (mine-path-info-visited target-info)
			      (setf (mine-path-info-distance target-info) (1+ (mine-path-info-distance source-info))
				    (mine-path-info-prev-site target-info) source
				    (mine-path-info-visited target-info) t)
			      (push target new-sources))))
			(site-neighbors source))))
	    sites)
    (when new-sources
      (map-path state new-sources mine-id))))

(defun print-state (state)
  (loop as mine in (state-mines state)
       do (format t "Mine ~d => S~d~%" (mine-mine-id mine) (mine-site-id mine)))
  (loop as site across (state-sites state)
       for i from 0
     do (if site
	    (format t "S~d -> ~{~d~^, ~}~%"
		    (site-id site)
		    (mapcar #'site-id (site-neighbors site)))
	    (format t "S~d -> blank~%" i))
     do (when site
	  (loop as mine-info across (site-mine-paths site)
	     as id from 0
	     do (format t "mine ~d score ~d distance ~A prev ~A~%"
			id
			(mine-path-info-score mine-info)
			(mine-path-info-distance mine-info)
			(when (mine-path-info-prev-site mine-info)
			  (site-id (mine-path-info-prev-site mine-info))))))))

(defun remove-river (state source-id target-id)
  (let* ((source (aref (state-sites state) source-id))
	 (target (aref (state-sites state) target-id))
	 (river-id (river-id source target)))
    (removef (state-rivers state) river-id :test #'equalp)
    (removef (site-neighbors source) target)
    (removef (site-neighbors target) source)
    (remap-path state source target)
    (remap-path state target source)
    state))

(defun remap-path (state source target)
  (loop as mine-info across (site-mine-paths source)
     as mine-id from 0
     do (when (eql (mine-path-info-prev-site mine-info) target)
	  (let ((start-sites (unvisit-nearest state source mine-info mine-id)))
	    (setf start-sites (remove-duplicates (remove nil start-sites :key (lambda (site) (mine-path-info-distance (site-mine-path-info site mine-id))))))
	    (setf start-sites (sort start-sites #'< :key (lambda (site) (mine-path-info-distance (site-mine-path-info site mine-id)))))
	    ;; Still a bit buggy - sites should be processed in groups of equal distances
	    (map-path state (copy-sequence 'list start-sites) mine-id)))))

(defun unvisit-nearest (state site mine-info mine-id &optional reacheable-neighbors)
  (setf (mine-path-info-visited mine-info) nil
	(mine-path-info-distance mine-info) nil
	(mine-path-info-prev-site mine-info) nil)
  (mapcar (lambda (neighbor)
	    (let ((mine-info (site-mine-path-info neighbor mine-id)))
	      (if (eql (mine-path-info-prev-site mine-info) site)
		  (setf reacheable-neighbors
			(unvisit-nearest state neighbor mine-info mine-id reacheable-neighbors))
		  (push neighbor reacheable-neighbors))))
	  (site-neighbors site))
  reacheable-neighbors)

(defun claim-river (state source-id target-id)
  (let* ((source (aref (state-sites state) source-id))
	 (target (aref (state-sites state) target-id))
	 (river-id (river-id source target))
	 (touched-networks (mapcan (lambda (network)
				     (when (or (find source-id (network-site-ids network))
					       (find target-id (network-site-ids network)))
				       (list network)))
				   (state-networks state))))
    (removef (state-rivers state) river-id :test #'equalp)
    (if touched-networks
	(if (second touched-networks)
	    (let ((first-network (first touched-networks))
		  (second-network (second touched-networks)))
	      (nunionf (network-site-ids first-network) (network-site-ids second-network))
	      (nunionf (network-sites first-network) (network-sites second-network))
	      (nunionf (network-mines first-network) (network-mines second-network))
	      (removef (state-networks state) second-network))
	    (let ((network (first touched-networks)))
	      (nunionf (network-site-ids network)
		       (list source-id target-id))
	      (nunionf (network-sites network)
		       (list source target))
	      (when-let ((mine (find source-id (state-mines state) :key #'mine-site-id)))
		(setf (network-mines network) (adjoin mine (network-mines network))))
	      (when-let ((mine (find target-id (state-mines state) :key #'mine-site-id)))
		(setf (network-mines network) (adjoin mine (network-mines network))))))
	(push (make-network :site-ids (list source-id target-id)
			    :sites (list source target)
			    :mines (mapcan (lambda (site-id)
					     (when-let ((mine (find site-id (state-mines state) :key #'mine-site-id)))
					       (list mine)))
					   (list source-id target-id)))
	      (state-networks state)))
    state))

(defun init-game (game-setup stream)
  (let ((state (load-map (assocr :map game-setup))))
    (setf (state-my-id state) (assocr :punter game-setup)
	  (state-n-punters state) (assocr :punters game-setup))
    (let ((data `(("ready" . ,(state-my-id state)))))
      (when *online-game*
	(push (cons "state" (let ((*print-readably* t)
				  (*print-circle* t))
			      (format nil "~A" state)))
	      data))
      (game-send data stream))
    state))

(defun play-game (server port strategy)
  (when-let ((socket (socket-connect server port)))
    (let ((stream (socket-stream socket))
	  (state nil))
      (game-send '(("me" . "zebiekste")) stream)
      (when (wait-for-input socket :timeout 5)
	(let ((response (game-decode (read-line stream))))
	  (unless (equalp (assocr :you response) "zebiekste")
	    (error "bad handshake; returned ~A" response))))
      (when (or (listen stream)
		(wait-for-input socket))
	(let ((game-setup (game-decode (read-line stream))))
	  (setf state (init-game game-setup stream))
	  (format t "Setup complete, our id is ~A, total punters ~A~%" (state-my-id state) (state-n-punters state))
	  #+nil(print-state state)))
      (loop
	 (when (wait-for-input socket :timeout 30)
	   (let ((update (game-decode (read-line stream))))
	     (if-let ((moves (assocr :moves (assocr :move update))))
	       (progn
		 (dolist (move moves)
		   (when-let ((claim (assocr :claim move)))
		     (let ((source-id (assocr :source claim))
			   (target-id (assocr :target claim))
			   (punter-id (assocr :punter claim)))
		       (unless (eql punter-id (state-my-id state))
			 (remove-river state source-id target-id)))))
		 (let* ((river (funcall strategy state))
			(source-id (car river))
			(target-id (cdr river)))
		   (if river
		       (progn
			 (claim-river state source-id target-id)
			 (game-send `(("claim" . (("punter" . ,(state-my-id state)) ("source" . ,source-id) ("target" . ,target-id)))) stream))
		       (game-send `(("pass" . (("punter" . ,(state-my-id state))))) stream))))
	       (let ((scores (assocr :scores (assocr :stop update))))
		 (format t "Server scores: ~%~:{Punter ~2d => ~4d~%~}" (mapcar (lambda (score)
										 (list (assocr :punter score) (assocr :score score)))
									       scores))
		 (format t "Our calculated score: ~d~%" (score-networks state))
		 #+nil(print-state state)
		 (return)))))))
    (socket-close socket)))

(defun strategy-pass (state)
  (declare (ignore state))
  nil)

(defun strategy-random (state)
  (elt (state-rivers state) (random (length (state-rivers state)))))

(defun strategy-greedy (state)
  (unless (state-strategy-data state)
    (setf (state-strategy-data state)
	  (coerce (state-sites state) 'list)))
  (let ((best-candidate nil)
	(best-score 0)
	(start-sites nil))
    (dolist (network (state-networks state))
      (dolist (site (network-sites network))
	(dolist (neighbor (site-neighbors site))
	  (unless (find neighbor (network-sites network))
	    (when (find (river-id site neighbor) (state-rivers state) :test #'equalp)
	      (let ((new-score (calc-claim-score state network neighbor)))
		(when (> new-score best-score)
		  (setf best-score new-score
			best-candidate (river-id site neighbor)))))))))
    (format t "Expected delta ~d~%" best-score)
    best-candidate))

(defun calc-claim-score (state origin site)
  (let ((score 0)
	(connected-network nil))
    (dolist (network (state-networks state))
      (unless (eq network origin)
	(when (find site (network-sites network))
	  (setf connected-network network))))
    (if connected-network
	(progn
	  (dolist (mine (network-mines origin))
	    (incf score (score-network connected-network (mine-mine-id mine))))
	  (dolist (mine (network-mines connected-network))
	    (incf score (score-network origin (mine-mine-id mine)))))
	(dolist (mine (network-mines origin))
	  (incf score (mine-path-info-score (site-mine-path-info site (mine-mine-id mine))))))
    score))

(defun game-send (data stream)
  (let ((json-string (json:encode-json-to-string data)))
    (format t "SENDING: ~d:~A" (length json-string) json-string)
    (format stream "~d:~A~%" (1+ (length json-string)) json-string)
    (finish-output stream)
    (format t " ok~%")))

(defun game-decode (string)
  (let ((delimiter-pos (position #\: string)))
    (let ((response (json:decode-json-from-string (subseq string (1+ delimiter-pos)))))
      #+nil(format t "RECEIVED: ~A~%" response)
      response)))

(defun score-networks (state)
  (let ((score 0))
    (dolist (network (state-networks state))
      (dolist (mine (network-mines network))
	(incf score (score-network network (mine-mine-id mine)))))
    score))

(defun score-network (network mine-id)
  (let ((score 0))
    (dolist (site (network-sites network))
      (let ((path-info (site-mine-path-info site mine-id)))
	(incf score (mine-path-info-score path-info))))
    score))
