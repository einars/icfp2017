(defpackage :punter/core
  (:use :cl :alexandria :usocket))

(in-package :punter/core)

(defparameter *offline-game* nil)

(defvar *moves*)

(defstruct (state (:type list)
		  (:constructor make-state-1))
  mines
  rivers
  sites
  networks
  strategy-data
  strategy-data-2
  my-id
  n-punters)

(defun main ()
  )

(defun make-state (num-sites)
  (make-state-1 :sites (make-array num-sites :initial-element nil)))

(defstruct (mine (:type list))
  site
  site-id
  mine-id
  potential)

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

(defun serialize-state (state)
  (let ((*print-circle* t)
	(*print-readably* t)
	(*print-pretty* nil))
    (with-output-to-string (out)
      (prin1 state out))))

(defun clone-state (state)
  (read-from-string (serialize-state state)))

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
			   :mine-id mine-id
			   :potential 0)
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
	       as mine in (state-mines state)
	       do (setf (mine-path-info-score mine-info)
			(if-let ((distance (mine-path-info-distance mine-info)))
			  (* distance distance)
			  0))
	       do (incf (mine-potential mine) (mine-path-info-score mine-info)))))
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
       do (format t "Mine ~d => S~d, potential ~d~%" (mine-mine-id mine) (mine-site-id mine) (mine-potential mine)))
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

(defun touched-networks (state river-id)
  (mapcan (lambda (network)
	    (when (or (find (car river-id) (network-site-ids network))
		      (find (cdr river-id) (network-site-ids network)))
	      (list network)))
	  (state-networks state)))

(defun claim-river (state source-id target-id)
  (let* ((source (aref (state-sites state) source-id))
	 (target (aref (state-sites state) target-id))
	 (river-id (river-id source target))
	 (touched-networks (touched-networks state river-id)))
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
      (when *offline-game*
	(push (cons "state" (let ((*print-readably* t)
				  (*print-circle* t))
			      (format nil "~A" state)))
	      data))
      (game-send data stream))
    state))

(defun play-game (server port strategy &key (name "zebiekste"))
  (when-let ((socket (socket-connect server port)))
    (let ((stream (socket-stream socket))
	  (state nil))
      (game-send `(("me" . ,name)) stream)
      (when (wait-for-input socket :timeout 5)
	(let ((response (game-decode (read-line stream))))
	  (unless (equalp (assocr :you response) name)
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
	     (if-let ((*moves* (assocr :moves (assocr :move update))))
	       (progn
		 (apply-moves state *moves*)
		 (let* ((river (funcall strategy state))
			(source-id (car river))
			(target-id (cdr river)))
		   (if river
		       (progn
			 #+nil(claim-river state source-id target-id)
			 (game-send `(("claim" . (("punter" . ,(state-my-id state)) ("source" . ,source-id) ("target" . ,target-id)))) stream))
		       (game-send `(("pass" . (("punter" . ,(state-my-id state))))) stream))))
	       (let ((scores (assocr :scores (assocr :stop update))))
		 (format t "Server scores: ~%~:{Punter ~2d => ~8d~%~}" (mapcar (lambda (score)
										 (list (assocr :punter score) (assocr :score score)))
									       scores))
		 (format t "Our calculated score: ~d~%" (score-networks state))
		 #+nil(print-state state)
		 (return)))))))
    (socket-close socket)))

(defun apply-moves (state moves)
  (dolist (move moves)
    (when-let ((claim (assocr :claim move)))
      (let ((source-id (assocr :source claim))
	    (target-id (assocr :target claim))
	    (punter-id (assocr :punter claim)))
	(if (eql punter-id (state-my-id state))
	    (claim-river state source-id target-id)
	    (remove-river state source-id target-id))))))

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
	(best-score 0))
    (dolist (network (state-networks state))
      (dolist (site (network-sites network))
	(dolist (neighbor (site-neighbors site))
	  (unless (find neighbor (network-sites network))
	    (when (find (river-id site neighbor) (state-rivers state) :test #'equalp)
	      (let ((new-score (calc-claim-score state network neighbor)))
		(when (> new-score best-score)
		  (setf best-score new-score
			best-candidate (river-id site neighbor)))))))))
    best-candidate))

(defun strategy-longest (state)
  (let ((best-candidate nil)
	(best-score 0)
	(best-mine-id nil))
    (loop as site across (state-sites state)
       do (when site
	    (unless (in-network-p state site)
	      (multiple-value-bind (site-score nearest-mine-id) (longest-score-site state site)
		(when (and site-score (> site-score best-score))
		  (let* ((init-river (next-shortest-path-river state site nearest-mine-id))
			 (init-site-id (if (find-if (lambda (network)
						      (find (car init-river) (network-site-ids network)))
						    (state-networks state))
					   (car init-river)
					   (cdr init-river)))
			 (adjusted-score (floor (+ site-score (longest-score-site state (aref (state-sites state) init-site-id)))
						2)))
		    (when (> adjusted-score best-score)
		      (setf best-score adjusted-score
			    best-candidate site
			    best-mine-id nearest-mine-id))))))))
    (if best-candidate
	(progn
	  (format t "Moving towards S~d for expected payout ~d, nearest mine L~d~%"
		  (site-id best-candidate)
		  best-score
		  best-mine-id)
	  (next-shortest-path-river state best-candidate best-mine-id))
	(strategy-greedy state))))

(defun longest-score-site (state site)
  (let ((site-score 0)
	(solution-exists nil)
	(nearest-mine-id nil)
	(nearest-mine-distance 100000000))
    (loop as mine in (strat-network-data-master-network (state-strategy-data state))
       as mine-id = (mine-mine-id mine)
       as mine-info = (aref (site-mine-paths site) mine-id)
       do (let ((score (mine-path-info-score mine-info))
		(distance (mine-path-info-distance mine-info)))
	    (when distance
	      (incf site-score score)
	      (setf solution-exists t)
	      (when (< distance nearest-mine-distance)
		(setf nearest-mine-id mine-id
		      nearest-mine-distance distance)))))
    (values (and solution-exists site-score)
	    nearest-mine-id)))

(defstruct (strat-evil-state (:type list))
  opponents)

(defstruct (opponent (:type list))
  id
  state)

(defun strategy-evil (state)
  (unless (state-strategy-data-2 state)
    (let ((setup (make-strat-evil-state))
	  (my-id (state-my-id state))
	  (opponents nil))
      (dotimes (i (state-n-punters state))
	(if (eql i my-id)
	    (push nil opponents)
	    (let ((opponent (make-opponent :id i :state (clone-state state))))
	      (push opponent opponents)
	      (setf (state-my-id (opponent-state opponent)) i))))
      (setf (strat-evil-state-opponents setup) (nreverse opponents))
      (setf (state-strategy-data-2 state) setup)))
  (let ((setup (state-strategy-data-2 state)))
    (dolist (opponent (strat-evil-state-opponents setup))
      (when opponent
	(apply-moves (opponent-state opponent) *moves*)))
    (dolist (opponent (strat-evil-state-opponents setup))
      (when opponent
	(let* ((opp-state (opponent-state opponent))
	       (best-river (strategy-greedy opp-state)))
	  (when (cdr (touched-networks opp-state best-river))
	    (format t "Performing DICK-MOVE on P~d~%" (opponent-id opponent))
	    (return-from strategy-evil best-river))))))
  (strategy-network state))

(defstruct (strat-network-data (:type list))
  master-network
  next-goal
  goal-mine
  origin
  alt-strategy
  anti-anti-move-state
  anti-anti-move-next-river
  anti-atni-move-goal)

(defun in-network-p (state site)
  (find-if (lambda (network)
	     (find site (network-sites network)))
	   (state-networks state)))

(defun strategy-network (state)
  (unless (state-strategy-data state)
    (setf (state-strategy-data state) (make-strat-network-data)))
  (let* ((setup (state-strategy-data state))
	 (master-network (strat-network-data-master-network setup))
	 (alt-strategy (strat-network-data-alt-strategy setup))
	 (next-goal (strat-network-data-next-goal setup)))
    (if alt-strategy
	(funcall alt-strategy state)
	(if master-network
	    (if next-goal
		(strat-network-advance state)
		(let ((best-candidate nil)
		      (first-mine nil)
		      (other-mine nil)
		      (shortest-path 1000000000))
		  (loop as mine2 in (state-mines state)
		     do (unless (find mine2 master-network)
			  (loop as mine1 in master-network
			     do (let ((path-info (site-mine-path-info (mine-site mine2) (mine-mine-id mine1))))
				  (when (and (mine-path-info-distance path-info)
					     (< (mine-path-info-distance path-info) shortest-path))
				    (setf shortest-path (mine-path-info-distance path-info)
					  best-candidate (mine-site mine2)
					  first-mine mine1
					  other-mine mine2))))))
		  (if best-candidate
		      (progn
			(setf (strat-network-data-next-goal setup) best-candidate)
			(setf (strat-network-data-goal-mine setup) other-mine)
			(setf (strat-network-data-origin setup) (mine-mine-id first-mine))
			(format t "Will try to join L~d with L~d, over distance ~d~%" (mine-mine-id other-mine) (mine-mine-id first-mine) shortest-path)
			(strat-network-advance state))
		      (progn
			(format t "Giving up, could not find joinable lambdas~%")
			(format t "Final network consists of ~d lambdas: ~{L~d~^, ~}~%"
				(length master-network)
				(mapcar #'mine-mine-id master-network))
			(setf (strat-network-data-alt-strategy setup) #'strategy-longest)
			(strategy-longest state)))))
	    (let ((best-candidate nil)
		  (first-mine nil)
		  (other-mine nil)
		  (max-potential 0)
		  (shortest-path 1000000000))
	      (loop as mine in (state-mines state)
		 do (let ((potential (mine-potential mine)))
		      (when (> potential max-potential)
			(setf first-mine mine
			      max-potential potential))))
	      (loop as mine2 in (state-mines state)
		 do (unless (eq mine2 first-mine)
		      (let ((path-info (site-mine-path-info (mine-site mine2) (mine-mine-id first-mine))))
			(when (and (mine-path-info-distance path-info)
				   (< (mine-path-info-distance path-info) shortest-path))
			  (setf shortest-path (mine-path-info-distance path-info)
				best-candidate (mine-site mine2)
				other-mine mine2)))))
	      (push first-mine (strat-network-data-master-network setup))
	      (if best-candidate
		  (progn
		    (setf (strat-network-data-next-goal setup) best-candidate)
		    (setf (strat-network-data-origin setup) (mine-mine-id first-mine))
		    (setf (strat-network-data-goal-mine setup) other-mine)
		    (format t "Will try to join L~d with L~d, over distance ~d~%" (mine-mine-id other-mine) (mine-mine-id first-mine) shortest-path)
		    (strat-network-advance state))
		  (progn
		    (format t "Giving up, could not find joinable lambdas~%")
		    (format t "Final network consists of ~d lambdas: ~{L~d~^, ~}~%"
			    (length master-network)
			    (mapcar #'mine-mine-id master-network))
		    (setf (strat-network-data-alt-strategy setup) #'strategy-longest)
		    (strategy-longest state))))))))

(defun strat-network-advance (state)
  (let* ((setup (state-strategy-data state))
	 (goal (strat-network-data-next-goal setup))
	 (origin (strat-network-data-origin setup)))
    (format t "Advancing network towards S~d from mine L~d~%" (site-id goal) origin)
    (when (and (eql (strat-network-data-anti-anti-move-state setup) 1)
	       (find (strat-network-data-anti-anti-move-next-river setup) (state-rivers state) :test #'equalp))
      (progn
	(setf (strat-network-data-anti-anti-move-state setup) 2
	      (strat-network-data-next-goal setup) (strat-network-data-anti-atni-move-goal setup)
	      (strat-network-data-origin setup) (mine-mine-id (strat-network-data-goal-mine setup))
	      (strat-network-data-goal-mine setup) (elt (state-mines state) origin))
	(return-from strat-network-advance (strat-network-data-anti-anti-move-next-river setup))))
    (multiple-value-bind (next-river curr-site next-site) (next-shortest-path-river state goal origin)
      (unless next-river
	(format t "Could not reach goal L~d, giving up~%" (mine-mine-id (strat-network-data-goal-mine setup)))
	(setf (strat-network-data-next-goal setup) nil
	      (strat-network-data-anti-anti-move-state setup) nil)
	(return-from strat-network-advance (strategy-network state)))
      (format t "Current anti-strat state is ~A~%" (strat-network-data-anti-anti-move-state setup))
      (unless (strat-network-data-anti-anti-move-state setup)
	(format t "Considering anti-strat at ~A / ~A~%" (site-id curr-site) (site-id next-site))
	(dolist (pot-source (site-neighbors next-site))
	  (unless (eq pot-source curr-site)
	    (when (and (find curr-site (site-neighbors pot-source))
		       (> (length (intersection (site-neighbors pot-source) (site-neighbors next-site))) 1))
	      (format t "Next river: ~A~%" (river-id curr-site pot-source))
	      (setf (strat-network-data-anti-anti-move-state setup) 1
		    (strat-network-data-anti-anti-move-next-river setup) (river-id curr-site pot-source)
		    (strat-network-data-anti-atni-move-goal setup) curr-site)))))
      (when (or (eql (site-id goal) (car next-river))
		(eql (site-id goal) (cdr next-river)))
	(setf (strat-network-data-next-goal setup) nil
	      (strat-network-data-anti-anti-move-state setup) nil)
	(format t "Reached L~d~%" (mine-mine-id (strat-network-data-goal-mine setup)))
	(push (strat-network-data-goal-mine setup)
	      (strat-network-data-master-network setup)))
      (if (find next-river (state-rivers state) :test #'equalp)
	  next-river
	  (strategy-network state)))))

(defun next-shortest-path-river (state site mine-id)
  (when site
    (let* ((path-info (site-mine-path-info site mine-id))
	   (prev-site (mine-path-info-prev-site path-info))
	   (joined nil))
      (dolist (network (state-networks state))
	(when (and (find mine-id (network-mines network) :key #'mine-mine-id)
		   (find prev-site (network-sites network)))
	  (setf joined t)))
      (if joined
	  (values (river-id site prev-site) prev-site site)
	  (next-shortest-path-river state prev-site mine-id)))))

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
