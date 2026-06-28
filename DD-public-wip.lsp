;;; duct_creator.lsp
;;; DD duct drawing routine with editable elbow blocks.
;;; JDaily signature build - public WIP
;;; Public WIP copy: company-specific path removed for GitHub
;;; Public commands: DD, DDB, DDV, DDSTYLE, DDREDLEN, DDE, DDR, DDHEAL, DDJOIN, DDFINAL

(vl-load-com)

(defun dd:3d (pt)
  (list (car pt) (cadr pt) (if (caddr pt) (caddr pt) 0.0))
)

(defun dd:2d (pt)
  (list (car pt) (cadr pt))
)

(defun dd:AngleDiff (a b / d)
  (setq d (abs (- a b)))
  (while (> d (* 2.0 pi))
    (setq d (- d (* 2.0 pi)))
  )
  (if (> d pi)
    (setq d (- (* 2.0 pi) d))
  )
  d
)

(defun dd:Min2 (a b)
  (if (< a b) a b)
)

(defun dd:Max2 (a b)
  (if (> a b) a b)
)

;;; Note: helper functions stay small so the geometry logic stays predictable.
(defun dd:MidPoint (p q)
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p q)
)

(defun dd:PointSegDistance (pt a b / ax ay bx by px py dx dy len2 ratio proj)
  (setq ax (car a)
        ay (cadr a)
        bx (car b)
        by (cadr b)
        px (car pt)
        py (cadr pt)
        dx (- bx ax)
        dy (- by ay)
        len2 (+ (* dx dx) (* dy dy)))
  (if (< len2 0.00000001)
    (distance pt a)
    (progn
      (setq ratio (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len2)
            ratio (dd:Max2 0.0 (dd:Min2 1.0 ratio))
            proj (list (+ ax (* ratio dx)) (+ ay (* ratio dy)) 0.0))
      (distance pt proj)
    )
  )
)

(defun dd:TransformByInsert (pt ed / ins sx sy rot x y xr yr)
  (setq ins (cdr (assoc 10 ed))
        sx (if (assoc 41 ed) (cdr (assoc 41 ed)) 1.0)
        sy (if (assoc 42 ed) (cdr (assoc 42 ed)) 1.0)
        rot (if (assoc 50 ed) (cdr (assoc 50 ed)) 0.0)
        x (* sx (car pt))
        y (* sy (cadr pt))
        xr (- (* x (cos rot)) (* y (sin rot)))
        yr (+ (* x (sin rot)) (* y (cos rot))))
  (list (+ (car ins) xr) (+ (cadr ins) yr) (+ (if (caddr ins) (caddr ins) 0.0) (if (caddr pt) (caddr pt) 0.0)))
)

(defun dd:NearestPoint (pt pts / best bestDist dist)
  (foreach p pts
    (setq dist (distance pt p))
    (if (or (not bestDist) (< dist bestDist))
      (setq best p
            bestDist dist)
    )
  )
  (list best bestDist)
)

(defun dd:ReplaceNth10 (dxf targetIndex newPt / idx out item)
  (setq idx -1
        out '())
  (foreach item dxf
    (if (= 10 (car item))
      (progn
        (setq idx (1+ idx))
        (if (= idx targetIndex)
          (setq out (append out (list (cons 10 (dd:2d newPt)))))
          (setq out (append out (list item)))
        )
      )
      (setq out (append out (list item)))
    )
  )
  out
)

(defun dd:PolylineEndpointInfo (ent / dxf pts lastIndex)
  (setq dxf (entget ent)
        pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) dxf)))
  (if pts
    (progn
      (setq lastIndex (1- (length pts)))
      (list
        (list ent "START" (dd:3d (car pts)) 0)
        (list ent "END" (dd:3d (last pts)) lastIndex)
      )
    )
  )
)

(defun dd:EntityEndpointInfo (ent / dxf typ)
  (setq dxf (entget ent)
        typ (cdr (assoc 0 dxf)))
  (cond
    ((= typ "LINE")
      (list
        (list ent "START" (dd:3d (cdr (assoc 10 dxf))) nil)
        (list ent "END" (dd:3d (cdr (assoc 11 dxf))) nil)
      ))
    ((= typ "LWPOLYLINE")
      (dd:PolylineEndpointInfo ent))
  )
)

(defun dd:EntitiesAfter (marker / ent out)
  (setq ent (if marker (entnext marker) (entnext))
        out '())
  (while ent
    (setq out (append out (list ent))
          ent (entnext ent))
  )
  out
)

(defun dd:SsToList (ss / idx out)
  (setq idx 0
        out '())
  (if ss
    (while (< idx (sslength ss))
      (setq out (append out (list (ssname ss idx)))
            idx (1+ idx))
    )
  )
  out
)

(defun dd:PlainTwoPointPlineData (ent / dxf flags pts bulges layer)
  (setq dxf (entget ent))
  (if (= "LWPOLYLINE" (cdr (assoc 0 dxf)))
    (progn
      (setq flags (if (assoc 70 dxf) (cdr (assoc 70 dxf)) 0)
            pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) dxf))
            bulges (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 42 (car x))) dxf))
            layer (cdr (assoc 8 dxf)))
      (if (and (= 2 (length pts))
               (= 0 (logand flags 1))
               (not (vl-some '(lambda (b) (not (equal b 0.0 0.000001))) bulges)))
        (list ent layer (dd:3d (car pts)) (dd:3d (last pts)))
      )
    )
  )
)

(defun dd:OppositeCollinear (shared far1 far2 / dist1 dist2 diff)
  (setq dist1 (distance shared far1)
        dist2 (distance shared far2))
  (if (and (> dist1 0.000001) (> dist2 0.000001))
    (progn
      (setq diff (dd:AngleDiff (angle shared far1) (angle shared far2)))
      (equal diff pi (/ pi 180.0))
    )
  )
)

(defun dd:MergePlinePairPoints (a b tol / p1 p2 q1 q2)
  (setq p1 (nth 2 a)
        p2 (nth 3 a)
        q1 (nth 2 b)
        q2 (nth 3 b))
  (cond
    ((and (equal p1 q1 tol) (dd:OppositeCollinear p1 p2 q2))
      (list p2 q2))
    ((and (equal p1 q2 tol) (dd:OppositeCollinear p1 p2 q1))
      (list p2 q1))
    ((and (equal p2 q1 tol) (dd:OppositeCollinear p2 p1 q2))
      (list p1 q2))
    ((and (equal p2 q2 tol) (dd:OppositeCollinear p2 p1 q1))
      (list p1 q1))
  )
)

(defun dd:SetTwoPointPline (ent p1 p2 / dxf)
  (setq dxf (entget ent))
  (if dxf
    (progn
      (setq dxf (dd:ReplaceNth10 dxf 0 p1)
            dxf (dd:ReplaceNth10 dxf 1 p2))
      (if (entmod dxf)
        (progn
          (entupd ent)
          T
        )
      )
    )
  )
)

;;; Note: straight joins get cleaned up before final output.
(defun dd:JoinCollinearPlineEdges (ents tol / changed count segs i j a b merged)
  (if (not tol) (setq tol 0.01))
  (setq changed T
        count 0)
  (while changed
    (setq changed nil
          segs (vl-remove nil (mapcar 'dd:PlainTwoPointPlineData ents))
          i 0)
    (while (and (not changed) (< i (length segs)))
      (setq a (nth i segs)
            j (1+ i))
      (while (and (not changed) (< j (length segs)))
        (setq b (nth j segs))
        (if (and (= (nth 1 a) (nth 1 b))
                 (setq merged (dd:MergePlinePairPoints a b tol)))
          (if (dd:SetTwoPointPline (car a) (car merged) (cadr merged))
            (progn
              (entdel (car b))
              (setq count (1+ count)
                    changed T)
            )
          )
        )
        (setq j (1+ j))
      )
      (setq i (1+ i))
    )
  )
  count
)

(defun dd:SetEntityEndpoint (ent which newPt idx / dxf typ)
  (setq dxf (entget ent)
        typ (cdr (assoc 0 dxf)))
  (cond
    ((= typ "LINE")
      (if (= which "START")
        (setq dxf (subst (cons 10 (dd:3d newPt)) (assoc 10 dxf) dxf))
        (setq dxf (subst (cons 11 (dd:3d newPt)) (assoc 11 dxf) dxf))
      )
      (entmod dxf)
      (entupd ent)
      T)
    ((and (= typ "LWPOLYLINE") idx)
      (entmod (dd:ReplaceNth10 dxf idx newPt))
      (entupd ent)
      T)
  )
)

(defun dd:MovePointByDelta (pt dx dy dz)
  (if (caddr pt)
    (list (+ (car pt) dx) (+ (cadr pt) dy) (+ (caddr pt) dz))
    (list (+ (car pt) dx) (+ (cadr pt) dy))
  )
)

(defun dd:MoveEntityPointToPoint (ent fromPt toPt / dxf typ dx dy dz out item code)
  (if (and ent fromPt toPt (not (equal fromPt toPt 0.000001)))
    (progn
      (setq dxf (entget ent)
            typ (cdr (assoc 0 dxf))
            dx (- (car toPt) (car fromPt))
            dy (- (cadr toPt) (cadr fromPt))
            dz (- (if (caddr toPt) (caddr toPt) 0.0) (if (caddr fromPt) (caddr fromPt) 0.0))
            out '())
      (foreach item dxf
        (setq code (car item))
        (if (or (and (= typ "LINE") (member code '(10 11)))
                (and (= typ "LWPOLYLINE") (= code 10)))
          (setq out (append out (list (cons code (dd:MovePointByDelta (cdr item) dx dy dz)))))
          (setq out (append out (list item)))
        )
      )
      (if (member typ '("LINE" "LWPOLYLINE"))
        (if (entmod out)
          (progn
            (entupd ent)
            T
          )
        )
      )
    )
  )
)

(defun dd:CurveEntityP (ent / typ)
  (if ent
    (progn
      (setq typ (cdr (assoc 0 (entget ent))))
      (member typ '("LINE" "LWPOLYLINE"))
    )
  )
)

(defun dd:CurveClosestPoint (ent pt / result)
  (setq result (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list ent pt)))
  (if (not (vl-catch-all-error-p result))
    result
  )
)

(defun dd:CurveParamAtPoint (ent pt / result)
  (setq result (vl-catch-all-apply 'vlax-curve-getParamAtPoint (list ent pt)))
  (if (not (vl-catch-all-error-p result))
    result
  )
)

(defun dd:CurvePointAtParam (ent param / result)
  (setq result (vl-catch-all-apply 'vlax-curve-getPointAtParam (list ent param)))
  (if (not (vl-catch-all-error-p result))
    result
  )
)

(defun dd:CurveTangentAngle (ent pt / param pBefore pAfter)
  (setq param (dd:CurveParamAtPoint ent pt))
  (if param
    (progn
      (setq pBefore (dd:CurvePointAtParam ent (dd:Max2 (vlax-curve-getStartParam ent) (- param 0.01)))
            pAfter (dd:CurvePointAtParam ent (dd:Min2 (vlax-curve-getEndParam ent) (+ param 0.01))))
      (if (and pBefore pAfter (> (distance pBefore pAfter) 0.000001))
        (angle pBefore pAfter)
      )
    )
  )
)

(defun dd:ParallelAngleP (a b tol / diff)
  (if (and a b)
    (progn
      (setq diff (dd:AngleDiff a b))
      (or (<= diff tol)
          (<= (abs (- pi diff)) tol))
    )
  )
)

(defun dd:FindOppositeDuctEdge (edgeEnt edgePt axisAng / ss idx ent pt ang dist bestEnt bestPt bestDist layer)
  (setq layer (cdr (assoc 8 (entget edgeEnt)))
        ss (ssget "_X" '((0 . "LINE,LWPOLYLINE")))
        idx 0)
  (if ss
    (while (< idx (sslength ss))
      (setq ent (ssname ss idx))
      (if (and (/= ent edgeEnt)
               (= layer (cdr (assoc 8 (entget ent))))
               (dd:CurveEntityP ent)
               (setq pt (dd:CurveClosestPoint ent edgePt))
               (setq ang (dd:CurveTangentAngle ent pt)))
        (progn
          (setq dist (distance edgePt pt))
          (if (and (> dist 0.5)
                   (< dist 120.0)
                   (dd:ParallelAngleP axisAng ang (/ pi 36.0))
                   (or (not bestDist) (< dist bestDist)))
            (setq bestEnt ent
                  bestDist dist
                  bestPt pt)
          )
        )
      )
      (setq idx (1+ idx))
    )
  )
  (if bestEnt
    (list bestEnt bestDist bestPt)
  )
)

(defun dd:EntmakeCircle (cen rad layer)
  (if (and cen rad (> rad 0.0))
    (entmakex
      (list
        '(0 . "CIRCLE")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        '(100 . "AcDbCircle")
        (cons 10 (dd:3d cen))
        (cons 40 rad)
      )
    )
  )
)

(defun dd:FirstSegmentAngle (ent / dxf typ pts p1 p2)
  (if ent
    (progn
      (setq dxf (entget ent)
            typ (cdr (assoc 0 dxf)))
      (cond
        ((= typ "LINE")
          (setq p1 (cdr (assoc 10 dxf))
                p2 (cdr (assoc 11 dxf))))
        ((= typ "LWPOLYLINE")
          (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) dxf))
                p1 (car pts)
                p2 (cadr pts)))
      )
      (if (and p1 p2 (> (distance p1 p2) 0.000001))
        (angle p1 p2)
      )
    )
  )
)

(defun dd:DrawBranchCollarLineAtAngle (tapData ang / center size layer seamCenter half perp pA pB)
  (if (and tapData ang)
    (progn
      (setq center (nth 0 tapData)
            size (nth 1 tapData)
            layer (nth 2 tapData))
      (if (and center size layer ang)
        (progn
          (setq half (/ size 2.0)
                seamCenter (polar center ang size)
                perp (+ ang (/ pi 2.0))
                pA (polar seamCenter perp half)
                pB (polar seamCenter (+ perp pi) half))
          (dd:EntmakePline (list pA pB) '(0.0 0.0) layer)
        )
      )
    )
  )
)

(defun dd:DrawBranchCollarLine (tapData routeEnt / ang)
  (setq ang (dd:FirstSegmentAngle routeEnt))
  (dd:DrawBranchCollarLineAtAngle tapData ang)
)

(defun dd:LineIntersectionAlong (a b c d / ip len ratio)
  (setq ip (inters a b c d nil))
  (if (and ip (> (setq len (distance a b)) 0.000001))
    (progn
      (setq ratio (/ (+ (* (- (car ip) (car a)) (- (car b) (car a)))
                        (* (- (cadr ip) (cadr a)) (- (cadr b) (cadr a))))
                     (* len len)))
      (if (and (> ratio 0.000001) (< ratio 0.999999))
        (list ratio ip)
      )
    )
  )
)

(defun dd:TwoPointCurveData (ent / dxf typ pts layer)
  (if ent
    (progn
      (setq dxf (entget ent)
            typ (cdr (assoc 0 dxf))
            layer (cdr (assoc 8 dxf)))
      (cond
        ((= typ "LINE")
          (list ent layer (dd:3d (cdr (assoc 10 dxf))) (dd:3d (cdr (assoc 11 dxf)))))
        ((= typ "LWPOLYLINE")
          (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 10 (car x))) dxf)))
          (if (= 2 (length pts))
            (list ent layer (dd:3d (car pts)) (dd:3d (cadr pts)))
          ))
      )
    )
  )
)

(defun dd:BreakLineAroundBranch (ent center routeAng branchSize gapExtra / data layer a b half perp bp1 bp2 side1 side2 hits sorted pLo pHi)
  (setq data (dd:TwoPointCurveData ent))
  (if data
    (progn
      (setq layer (nth 1 data)
            a (nth 2 data)
            b (nth 3 data)
            half (/ branchSize 2.0)
            perp (+ routeAng (/ pi 2.0))
            bp1 (polar center perp (+ half gapExtra))
            bp2 (polar center (+ perp pi) (+ half gapExtra))
            side1 (polar bp1 routeAng 1.0)
            side2 (polar bp2 routeAng 1.0)
            hits
              (vl-remove
                nil
                (list
                  (dd:LineIntersectionAlong a b bp1 side1)
                  (dd:LineIntersectionAlong a b bp2 side2)
                )))
      (if (= 2 (length hits))
        (progn
          (setq sorted (vl-sort hits '(lambda (x y) (< (car x) (car y))))
                pLo (cadr (car sorted))
                pHi (cadr (cadr sorted)))
          (entdel ent)
          (if (> (distance a pLo) 0.000001)
            (dd:EntmakePline (list a pLo) '(0.0 0.0) layer)
          )
          (if (> (distance pHi b) 0.000001)
            (dd:EntmakePline (list pHi b) '(0.0 0.0) layer)
          )
          T
        )
      )
    )
  )
)

(defun dd:EdgeRouteProjection (ent center routeAng / pt)
  (setq pt (dd:CurveClosestPoint ent center))
  (if pt
    (+ (* (- (car pt) (car center)) (cos routeAng))
       (* (- (cadr pt) (cadr center)) (sin routeAng)))
  )
)

(defun dd:BreakMainEdgesForBranch (tapData routeAng / center size edges count ent proj breakEnt breakProj)
  (setq count 0)
  (if (and tapData routeAng)
    (progn
      (setq center (nth 0 tapData)
            size (nth 1 tapData)
            edges (nth 3 tapData))
      (foreach ent edges
        (setq proj (dd:EdgeRouteProjection ent center routeAng))
        (if (and proj (or (not breakProj) (> proj breakProj)))
          (setq breakEnt ent
                breakProj proj)
        )
      )
      (if breakEnt
        (if (dd:BreakLineAroundBranch breakEnt center routeAng size 0.0)
          (setq count 1)
        )
      )
    )
  )
  count
)

(defun dd:CornerBulges (a1 a2 / b23 b41 tAng)
  (setq tAng (- a2 a1))
  (if (minusp tAng) (setq tAng (- tAng)))
  (if (and (< 0 tAng) (>= pi tAng))
    (setq b23 (/ (- a2 a1) 4.0)
          b41 (/ (- a1 a2) 4.0))
    (progn
      (if (< a1 a2)
        (setq a1 (+ a1 pi)
              a2 (- a2 pi))
        (setq a1 (- a1 pi)
              a2 (+ a2 pi))
      )
      (setq b23 (/ (- a2 a1) 4.0)
            b41 (/ (- a1 a2) 4.0))
    )
  )
  (list 0.0 b23 0.0 b41)
)

(defun dd:ResolveElbowStyle (ang1 ang2 / style)
  (setq style (if dd:elbowStyle (strcase dd:elbowStyle) "AUTO"))
  (if (= style "AUTO")
    (if (equal (dd:AngleDiff ang1 ang2) (/ pi 2.0) (/ pi 36.0))
      "SQUARE"
      "RADIUS"
    )
    style
  )
)

(defun dd:NextElbowBlockName (/ name)
  (if (not dd:elbowBlockNumber) (setq dd:elbowBlockNumber 1))
  (setq name (strcat "DD_ELBOW_" (itoa dd:elbowBlockNumber)))
  (while (tblsearch "BLOCK" name)
    (setq dd:elbowBlockNumber (1+ dd:elbowBlockNumber)
          name (strcat "DD_ELBOW_" (itoa dd:elbowBlockNumber)))
  )
  (setq dd:elbowBlockNumber (1+ dd:elbowBlockNumber))
  name
)

(defun dd:NextReducerBlockName (/ name)
  (if (not dd:reducerBlockNumber) (setq dd:reducerBlockNumber 1))
  (setq name (strcat "DD_REDUCER_" (itoa dd:reducerBlockNumber)))
  (while (tblsearch "BLOCK" name)
    (setq dd:reducerBlockNumber (1+ dd:reducerBlockNumber)
          name (strcat "DD_REDUCER_" (itoa dd:reducerBlockNumber)))
  )
  (setq dd:reducerBlockNumber (1+ dd:reducerBlockNumber))
  name
)

(defun dd:EntmakePline (pts bulges layer / dxf idx p b)
  (setq dxf
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))
      '(70 . 0)
      '(43 . 0.0)
    )
    idx 0
  )
  (foreach p pts
    (setq b (if (nth idx bulges) (nth idx bulges) 0.0)
          dxf (append dxf (list (cons 10 (dd:2d p)) (cons 42 b)))
          idx (1+ idx))
  )
  (entmake dxf)
)

(defun dd:RoundUpToSixteenth (val / scaled whole)
  (setq scaled (* val 16.0)
        whole (fix scaled))
  (/ (if (equal scaled (float whole) 0.000001) whole (1+ whole)) 16.0)
)

(defun dd:DrawVcdSymbol (edgePt axisAng size layer opposite / marker offset handle bladeAng bladeEnd offsetPt offsetEnd handleEnd ents)
  (setq marker (entlast)
        offset (dd:RoundUpToSixteenth (* size 0.21))
        handle (dd:RoundUpToSixteenth (* size 0.6484375))
        bladeAng (if opposite
                   (angle edgePt (nth 2 opposite))
                   (+ axisAng (/ pi 2.0)))
        bladeEnd (polar edgePt bladeAng size)
        offsetPt (polar edgePt axisAng offset)
        offsetEnd (polar bladeEnd axisAng offset)
        handleEnd (polar offsetEnd bladeAng handle))
  (dd:EntmakePline (list edgePt bladeEnd) '(0.0 0.0) layer)
  (dd:EntmakePline (list edgePt offsetPt) '(0.0 0.0) layer)
  (dd:EntmakePline (list bladeEnd offsetEnd) '(0.0 0.0) layer)
  (dd:EntmakePline (list offsetPt handleEnd) '(0.0 0.0) layer)
  (setq ents (dd:EntitiesAfter marker))
  (list ents offset handle (+ size handle))
)

(defun dd:TurnSign (dir1 dir2 / raw)
  (setq raw (- dir2 dir1))
  (while (> raw pi) (setq raw (- raw (* 2.0 pi))))
  (while (< raw (- pi)) (setq raw (+ raw (* 2.0 pi))))
  (if (minusp raw) -1.0 1.0)
)

(defun dd:UnitVectorPoint (fromPt toPt / dist)
  (setq dist (distance fromPt toPt))
  (if (> dist 0.000001)
    (list
      (/ (- (car toPt) (car fromPt)) dist)
      (/ (- (cadr toPt) (cadr fromPt)) dist)
      0.0
    )
    '(0.0 0.0 0.0)
  )
)

(defun dd:OffsetPointByVector (pt vec dist)
  (list
    (+ (car pt) (* (car vec) dist))
    (+ (cadr pt) (* (cadr vec) dist))
    (if (caddr pt) (caddr pt) 0.0)
  )
)

(defun dd:ProjectionDistance (origin pt unit)
  (+ (* (- (car pt) (car origin)) (car unit))
     (* (- (cadr pt) (cadr origin)) (cadr unit)))
)

(defun dd:OffsetAwayFromPoint (pt vec dist awayPt / plus minus)
  (setq plus (dd:OffsetPointByVector pt vec dist)
        minus (dd:OffsetPointByVector pt vec (- dist)))
  (if (> (distance plus awayPt) (distance minus awayPt))
    plus
    minus
  )
)

(defun dd:BulgeAwayFromPoint (p1 p2 bulge awayPt / mid chord left midPlus midMinus)
  (setq mid (dd:MidPoint p1 p2)
        chord (distance p1 p2)
        left (+ (angle p1 p2) (/ pi 2.0))
        midPlus (polar mid left (* (abs bulge) (/ chord 2.0)))
        midMinus (polar mid (+ left pi) (* (abs bulge) (/ chord 2.0))))
  (if (> (distance midPlus awayPt) (distance midMinus awayPt))
    (abs bulge)
    (- (abs bulge))
  )
)

(defun dd:BulgeTowardPoint (p1 p2 bulge targetPt / mid chord left midPlus midMinus)
  (setq mid (dd:MidPoint p1 p2)
        chord (distance p1 p2)
        left (+ (angle p1 p2) (/ pi 2.0))
        midPlus (polar mid left (* (abs bulge) (/ chord 2.0)))
        midMinus (polar mid (+ left pi) (* (abs bulge) (/ chord 2.0))))
  (if (< (distance midPlus targetPt) (distance midMinus targetPt))
    (abs bulge)
    (- (abs bulge))
  )
)

(defun dd:BulgeMidPoint (p1 p2 bulge / mid chord left)
  (setq mid (dd:MidPoint p1 p2)
        chord (distance p1 p2)
        left (+ (angle p1 p2) (/ pi 2.0)))
  (if (minusp bulge)
    (polar mid (+ left pi) (* (abs bulge) (/ chord 2.0)))
    (polar mid left (* (abs bulge) (/ chord 2.0)))
  )
)

(defun dd:ReducerLength ()
  (if (and *DD_REDUCER_LENGTH* (> *DD_REDUCER_LENGTH* 0.0))
    *DD_REDUCER_LENGTH*
    6.0
  )
)

(defun dd:PlineVertices (ent / data defaultWidth out pt sw ew bulge)
  (setq data (entget ent)
        defaultWidth (if (assoc 43 data) (cdr (assoc 43 data)) 0.0)
        out '())
  (while data
    (if (= 10 (caar data))
      (progn
        (setq pt (cdr (car data))
              sw defaultWidth
              ew defaultWidth
              bulge 0.0
              data (cdr data))
        (while (and data (/= 10 (caar data)))
          (cond
            ((= 40 (caar data)) (setq sw (cdar data)))
            ((= 41 (caar data)) (setq ew (cdar data)))
            ((= 42 (caar data)) (setq bulge (cdar data)))
          )
          (setq data (cdr data))
        )
        (setq out (append out (list (list (dd:3d pt) sw ew bulge))))
      )
      (setq data (cdr data))
    )
  )
  out
)

(defun dd:SetVertexEndWidth (vtx width)
  (list (car vtx) (cadr vtx) width (nth 3 vtx))
)

(defun dd:AppendVertex (vtxLst vtx / lastVtx)
  (setq lastVtx (car (reverse vtxLst)))
  (if (and lastVtx (equal (car lastVtx) (car vtx) 0.000001))
    (append (reverse (cdr (reverse vtxLst))) (list vtx))
    (append vtxLst (list vtx))
  )
)

(defun dd:MakeCenterlinePline (vtxLst layer / dxf vtx)
  (setq dxf
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbPolyline")
      (cons 90 (length vtxLst))
      '(70 . 0)
    )
  )
  (foreach vtx vtxLst
    (setq dxf
      (append dxf
        (list
          (cons 10 (dd:2d (car vtx)))
          (cons 40 (cadr vtx))
          (cons 41 (nth 2 vtx))
          (cons 42 (nth 3 vtx))
        )
      )
    )
  )
  (if (entmake dxf)
    (entlast)
  )
)

(defun dd:ApplyReducerToPline (ent pickPt newWidth / closePt hitDist verts layer
                                    accum idx segCount v1 v2 p1 p2 len offset
                                    oldStart oldEnd oldWidth redLen redStart redEnd
                                    newVerts applied after)
  (setq closePt (vlax-curve-getClosestPointTo ent pickPt)
        hitDist (vlax-curve-getDistAtPoint ent closePt)
        verts (dd:PlineVertices ent)
        layer (cdr (assoc 8 (entget ent)))
        accum 0.0
        idx 0
        segCount (1- (length verts))
        newVerts (list (car verts))
        applied nil
        after nil)
  (while (< idx segCount)
    (setq v1 (nth idx verts)
          v2 (nth (1+ idx) verts)
          p1 (car v1)
          p2 (car v2)
          len (distance p1 p2))
    (cond
      (after
        (setq newVerts (dd:AppendVertex newVerts (list p2 newWidth newWidth 0.0))))
      ((and (not applied)
            (> len 0.000001)
            (<= hitDist (+ accum len 0.000001)))
        (setq offset (dd:Max2 0.0 (dd:Min2 len (- hitDist accum)))
              oldStart (cadr v1)
              oldEnd (nth 2 v1)
              oldWidth (+ oldStart (* (/ offset len) (- oldEnd oldStart)))
              redLen (dd:Min2 (dd:ReducerLength) (- len offset))
              redStart (polar p1 (angle p1 p2) offset)
              redEnd (polar redStart (angle p1 p2) redLen))
        (if (> offset 0.000001)
          (progn
            (setq newVerts
              (append (reverse (cdr (reverse newVerts)))
                      (list (dd:SetVertexEndWidth (car (reverse newVerts)) oldWidth))))
            (setq newVerts
              (dd:AppendVertex newVerts (list redStart oldWidth newWidth 0.0)))
          )
          (setq newVerts
            (append (reverse (cdr (reverse newVerts)))
                    (list (list p1 oldWidth newWidth 0.0))))
        )
        (if (> redLen 0.000001)
          (setq newVerts
            (dd:AppendVertex newVerts (list redEnd newWidth newWidth 0.0)))
        )
        (if (> (distance redEnd p2) 0.000001)
          (setq newVerts
            (dd:AppendVertex newVerts (list p2 newWidth newWidth 0.0)))
        )
        (setq applied T
              after T))
      (T
        (setq newVerts (dd:AppendVertex newVerts v2)))
    )
    (setq accum (+ accum len)
          idx (1+ idx))
  )
  (if applied
    (progn
      (entdel ent)
      (dd:MakeCenterlinePline newVerts layer)
    )
    ent
  )
)

(defun dd:EntmakeElbowVanes (pts dir1 dir2 layer / p1 p2 p3 p4 i1 i2 c1 c2 ctr inner outer diagLen unit
                                      n width rad edgeClear startDist endDist usable i frac cen u1 u2 pA pB
                                      baseBulge bulge testA testB testBulge testMid minOff maxOff proj)
  (if (= 4 (length pts))
    (progn
      (setq p1 (nth 0 pts)
            p2 (nth 1 pts)
            p3 (nth 2 pts)
            p4 (nth 3 pts)
            i1 (inters p1 (polar p1 dir1 1.0) p4 (polar p4 dir2 1.0) nil)
            i2 (inters p2 (polar p2 dir1 1.0) p3 (polar p3 dir2 1.0) nil)
            c1 (dd:MidPoint p1 p2)
            c2 (dd:MidPoint p3 p4)
            ctr (inters c1 (polar c1 dir1 1.0) c2 (polar c2 dir2 1.0) nil))
      (if (and i1 i2 ctr)
        (progn
          (if (< (distance ctr i1) (distance ctr i2))
            (setq inner i1 outer i2)
            (setq inner i2 outer i1)
          )
          (setq width (distance p1 p2)
                n 5
                rad (dd:Max2 0.45 (/ width 12.0))
                diagLen (distance inner outer)
                unit (dd:UnitVectorPoint inner outer)
                u1 (list (cos dir1) (sin dir1) 0.0)
                u2 (list (cos dir2) (sin dir2) 0.0)
                baseBulge (* (dd:TurnSign dir1 dir2) (/ (sin (/ pi 8.0)) (cos (/ pi 8.0))))
                i 0)
          (setq testA (dd:OffsetAwayFromPoint inner u2 rad outer)
                testB (dd:OffsetAwayFromPoint inner u1 rad outer)
                testBulge (dd:BulgeTowardPoint testA testB baseBulge outer)
                testMid (dd:BulgeMidPoint testA testB testBulge)
                minOff nil
                maxOff nil)
          (foreach proj
            (list
              (dd:ProjectionDistance inner testA unit)
              (dd:ProjectionDistance inner testB unit)
              (dd:ProjectionDistance inner testMid unit))
            (setq minOff (if minOff (dd:Min2 minOff proj) proj)
                  maxOff (if maxOff (dd:Max2 maxOff proj) proj))
          )
          (setq edgeClear (dd:Max2 0.45 (* rad 0.85))
                startDist (- edgeClear minOff)
                endDist (- diagLen edgeClear maxOff)
                usable (- endDist startDist))
          (if (> usable 0.000001)
            (while (< i n)
              (setq frac (if (= n 1) 0.5 (/ (float i) (1- n)))
                    cen (dd:OffsetPointByVector inner unit (+ startDist (* frac usable)))
                    pA (dd:OffsetAwayFromPoint cen u2 rad outer)
                    pB (dd:OffsetAwayFromPoint cen u1 rad outer)
                    bulge (dd:BulgeTowardPoint pA pB baseBulge outer))
              (dd:EntmakePline (list pA pB) (list bulge 0.0) layer)
              (setq i (1+ i))
            )
          )
        )
      )
    )
  )
)

(defun dd:EntmakeElbowGeometry (style pts dir1 dir2 layer vanes / p1 p2 p3 p4 i1 i2 bulges)
  (setq p1 (nth 0 pts)
        p2 (nth 1 pts)
        p3 (nth 2 pts)
        p4 (nth 3 pts)
        style (strcase style))
  (if (= style "SQUARE")
    (progn
      (setq i1 (inters p1 (polar p1 dir1 1.0) p4 (polar p4 dir2 1.0) nil)
            i2 (inters p2 (polar p2 dir1 1.0) p3 (polar p3 dir2 1.0) nil))
      (if (and i1 i2)
        (progn
          (dd:EntmakePline (list p1 i1 p4) '(0.0 0.0 0.0) layer)
          (dd:EntmakePline (list p2 i2 p3) '(0.0 0.0 0.0) layer)
          (dd:EntmakePline (list p1 p2) '(0.0 0.0) layer)
          (dd:EntmakePline (list p3 p4) '(0.0 0.0) layer)
          (if vanes (dd:EntmakeElbowVanes pts dir1 dir2 layer))
        )
        (progn
          (dd:EntmakePline (list p1 p2) '(0.0 0.0) layer)
          (dd:EntmakePline (list p2 p3) '(0.0 0.0) layer)
          (dd:EntmakePline (list p3 p4) '(0.0 0.0) layer)
          (dd:EntmakePline (list p4 p1) '(0.0 0.0) layer)
          (if vanes (dd:EntmakeElbowVanes pts dir1 dir2 layer))
        )
      )
    )
    (progn
      (setq bulges (dd:CornerBulges (+ dir1 (/ pi 2.0)) (+ dir2 (/ pi 2.0))))
      (dd:EntmakePline (list p1 p2) (list (nth 0 bulges) 0.0) layer)
      (dd:EntmakePline (list p2 p3) (list (nth 1 bulges) 0.0) layer)
      (dd:EntmakePline (list p3 p4) (list (nth 2 bulges) 0.0) layer)
      (dd:EntmakePline (list p4 p1) (list (nth 3 bulges) 0.0) layer)
    )
  )
)

(defun dd:SetElbowXData (ent style pts dir1 dir2 vanes / xdata)
  (regapp "DD_ELBOW")
  (setq xdata
    (append
      (list "DD_ELBOW" (cons 1000 (strcase style)) (cons 1070 (if vanes 1 0)) (cons 1040 dir1) (cons 1040 dir2))
      (mapcar '(lambda (pt) (cons 1010 (dd:3d pt))) pts)
    )
  )
  (entmod (append (entget ent) (list (list -3 xdata))))
  (entupd ent)
  ent
)

(defun dd:CreateElbowBlock (pts style dir1 dir2 layer vanes / name ins)
  (setq style (strcase style)
        name (dd:NextElbowBlockName))
  (if (/= style "SQUARE") (setq vanes nil))
  (entmake
    (list
      '(0 . "BLOCK")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbBlockBegin")
      (cons 2 name)
      '(70 . 0)
      '(10 0.0 0.0 0.0)
    )
  )
  (dd:EntmakeElbowGeometry style pts dir1 dir2 layer vanes)
  (entmake
    (list
      '(0 . "ENDBLK")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbBlockEnd")
    )
  )
  (setq ins
    (entmakex
      (list
        '(0 . "INSERT")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        '(100 . "AcDbBlockReference")
        (cons 2 name)
        '(10 0.0 0.0 0.0)
        '(41 . 1.0)
        '(42 . 1.0)
        '(43 . 1.0)
        '(50 . 0.0)
      )
    )
  )
  (if ins (dd:SetElbowXData ins style pts dir1 dir2 vanes))
  ins
)

(defun dd:ReducerCentersFromPts (pts)
  (if (= 4 (length pts))
    (list
      (dd:MidPoint (nth 0 pts) (nth 1 pts))
      (dd:MidPoint (nth 2 pts) (nth 3 pts))
    )
  )
)

(defun dd:ReducerPtsForStyle (c1 c2 w1 w2 style / axisAng perp outC2)
  (setq style (strcase style)
        axisAng (angle c1 c2)
        perp (+ (/ pi 2.0) axisAng)
        outC2
          (cond
            ((= style "SIDEA")
              (polar c2 perp (/ (- w1 w2) 2.0)))
            ((= style "SIDEB")
              (polar c2 (+ pi perp) (/ (- w1 w2) 2.0)))
            (T c2)))
  (list
    (polar c1 perp (/ w1 2.0))
    (polar c1 (+ pi perp) (/ w1 2.0))
    (polar outC2 (+ pi perp) (/ w2 2.0))
    (polar outC2 perp (/ w2 2.0))
  )
)

(defun dd:ReducerStyleFromPick (pts pick / dA dB)
  (if (and (= 4 (length pts)) pick)
    (progn
      (setq dA (dd:PointSegDistance pick (nth 0 pts) (nth 3 pts))
            dB (dd:PointSegDistance pick (nth 1 pts) (nth 2 pts)))
      (if (<= dA dB) "SIDEA" "SIDEB")
    )
    "SIDEA"
  )
)

(defun dd:ReducerStyleFromDiagonalPick (pts pick / style)
  (setq style (dd:ReducerStyleFromPick pts pick))
  (cond
    ((= style "SIDEA") "SIDEB")
    ((= style "SIDEB") "SIDEA")
    (T "SIDEA")
  )
)

(defun dd:SetReducerXData (ent pts w1 w2 style centers / xdata)
  (regapp "DD_REDUCER")
  (if (not style) (setq style "BOTH"))
  (if (not centers) (setq centers (dd:ReducerCentersFromPts pts)))
  (setq xdata
    (append
      (list "DD_REDUCER" (cons 1000 (strcase style)) (cons 1040 w1) (cons 1040 w2))
      (mapcar '(lambda (pt) (cons 1010 (dd:3d pt))) pts)
      (mapcar '(lambda (pt) (cons 1011 (dd:3d pt))) centers)
    )
  )
  (entmod (append (entget ent) (list (list -3 xdata))))
  (entupd ent)
  ent
)

(defun dd:EntmakeReducerGeometry (pts layer / p1 p2 p3 p4)
  (if (= 4 (length pts))
    (progn
      (setq p1 (nth 0 pts)
            p2 (nth 1 pts)
            p3 (nth 2 pts)
            p4 (nth 3 pts))
      (dd:EntmakePline (list p1 p2) '(0.0 0.0) layer)
      (dd:EntmakePline (list p2 p3) '(0.0 0.0) layer)
      (dd:EntmakePline (list p3 p4) '(0.0 0.0) layer)
      (dd:EntmakePline (list p4 p1) '(0.0 0.0) layer)
    )
  )
)

(defun dd:CreateReducerBlock (pts w1 w2 layer style centers / name ins)
  (setq name (dd:NextReducerBlockName))
  (entmake
    (list
      '(0 . "BLOCK")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbBlockBegin")
      (cons 2 name)
      '(70 . 0)
      '(10 0.0 0.0 0.0)
    )
  )
  (dd:EntmakeReducerGeometry pts layer)
  (entmake
    (list
      '(0 . "ENDBLK")
      '(100 . "AcDbEntity")
      (cons 8 layer)
      '(100 . "AcDbBlockEnd")
    )
  )
  (setq ins
    (entmakex
      (list
        '(0 . "INSERT")
        '(100 . "AcDbEntity")
        (cons 8 layer)
        '(100 . "AcDbBlockReference")
        (cons 2 name)
        '(10 0.0 0.0 0.0)
        '(41 . 1.0)
        '(42 . 1.0)
        '(43 . 1.0)
        '(50 . 0.0)
      )
    )
  )
  (if ins (dd:SetReducerXData ins pts w1 w2 style centers))
  ins
)

;;; Note: reducer rebuilds depend on consistent block metadata.
(defun dd:GetReducerData (ent / ed app data vals pts centers style)
  (setq ed (entget ent '("DD_REDUCER"))
        app (assoc -3 ed))
  (if app
    (progn
      (setq data (car (cdr app))
            vals (cdr data)
            style (if (assoc 1000 vals) (cdr (assoc 1000 vals)) "BOTH")
            pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 1010 (car x))) vals))
            centers (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 1011 (car x))) vals))
            vals (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 1040 (car x))) vals)))
      (if (/= 2 (length centers))
        (setq centers (dd:ReducerCentersFromPts pts))
      )
      (if (= "INSERT" (cdr (assoc 0 ed)))
        (progn
          (setq pts (mapcar '(lambda (pt) (dd:TransformByInsert pt ed)) pts))
          (setq centers (mapcar '(lambda (pt) (dd:TransformByInsert pt ed)) centers))
        )
      )
      (if (and (= 2 (length vals)) (= 4 (length pts)))
        (list (nth 0 vals) (nth 1 vals) pts (strcase style) centers)
      )
    )
  )
)

(defun dd:HealReducerDucts (ent tol ss / data pts idx item endpoints endpoint near snap dist
                                bestEndpoint bestSnap bestDist count)
  (setq data (dd:GetReducerData ent)
        count 0)
  (if data
    (progn
      (setq pts (nth 2 data))
      (if (not tol) (setq tol (dd:HealTolerance)))
      (if (not ss)
        (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE"))))
      )
      (if ss
        (progn
          (setq idx 0)
          (while (< idx (sslength ss))
            (setq item (ssname ss idx)
                  endpoints (dd:EntityEndpointInfo item)
                  bestEndpoint nil
                  bestSnap nil
                  bestDist nil)
            (foreach endpoint endpoints
              (setq near (dd:NearestPoint (nth 2 endpoint) pts)
                    snap (car near)
                    dist (cadr near))
              (if (and snap dist (<= dist tol)
                       (or (not bestDist) (< dist bestDist)))
                (setq bestEndpoint endpoint
                      bestSnap snap
                      bestDist dist)
              )
            )
            (if (and bestEndpoint bestSnap)
              (if (dd:MoveEntityPointToPoint item (nth 2 bestEndpoint) bestSnap)
                (setq count (1+ count))
              )
            )
            (setq idx (1+ idx))
          )
        )
      )
    )
  )
  count
)

(defun dd:HealTolerance ()
  (if (and *DD_HEAL_TOLERANCE* (> *DD_HEAL_TOLERANCE* 0.0))
    *DD_HEAL_TOLERANCE*
    12.0
  )
)

(defun dd:PromptHealTolerance (/ tol)
  (setq tol
    (getdist
      (strcat
        "\nReconnect search tolerance <"
        (rtos (dd:HealTolerance) 2 2)
        ">: "
      )
    )
  )
  (if tol
    (if (> tol 0.0)
      (setq *DD_HEAL_TOLERANCE* tol)
      (princ "\nTolerance must be greater than 0.")
    )
  )
  (dd:HealTolerance)
)

(defun dd:PromptHealSelection (/ ss)
  (princ "\nSelect duct lines/polylines to move to reducer, or press Enter for nearby auto-search: ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE"))))
  ss
)

(defun dd:RebuildReducer (ent style / data layer w1 w2 centers pts)
  (setq data (dd:GetReducerData ent))
  (if data
    (progn
      (setq layer (cdr (assoc 8 (entget ent)))
            w1 (car data)
            w2 (cadr data)
            centers (nth 4 data)
            pts (dd:ReducerPtsForStyle (car centers) (cadr centers) w1 w2 style))
      (entdel ent)
      (dd:CreateReducerBlock pts w1 w2 layer style centers)
    )
  )
)

(defun dd:GetElbowData (ent / ed app data style vals pts vaneVal vanes)
  (setq ed (entget ent '("DD_ELBOW"))
        app (assoc -3 ed))
  (if app
    (progn
      (setq data (car (cdr app))
            vals (cdr data)
            style (cdr (assoc 1000 vals))
            vaneVal (assoc 1070 vals)
            vanes (and vaneVal (= 1 (cdr vaneVal)))
            pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 1010 (car x))) vals))
            vals (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= 1040 (car x))) vals)))
      (if (and style (= 2 (length vals)) (= 4 (length pts)))
        (list (strcase style) (nth 0 vals) (nth 1 vals) pts vanes)
      )
    )
  )
)

(defun dd:RebuildElbow (ent style vanes / data layer)
  (setq data (dd:GetElbowData ent))
  (if data
    (progn
      (setq layer (cdr (assoc 8 (entget ent))))
      (entdel ent)
      (dd:CreateElbowBlock (nth 3 data) style (nth 1 data) (nth 2 data) layer vanes)
    )
  )
)

(defun dd:ElbowsFromSelection (ss / idx ent out)
  (setq idx 0
        out '())
  (if ss
    (while (< idx (sslength ss))
      (setq ent (ssname ss idx))
      (if (dd:GetElbowData ent)
        (setq out (append out (list ent)))
      )
      (setq idx (1+ idx))
    )
  )
  out
)

(defun dd:FirstElbowStyle (elbows / data)
  (if elbows
    (progn
      (setq data (dd:GetElbowData (car elbows)))
      (if data (car data) "RADIUS")
    )
    "RADIUS"
  )
)

(defun dd:ApplyElbowEdit (elbows opt / count ent newEnt oldCmdecho data style vanes)
  (setq count 0
        opt (strcase opt))
  (if (= "EXPLODE" opt)
    (progn
      (setq oldCmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (foreach ent elbows
        (if (dd:GetElbowData ent)
          (progn
            (command "_.explode" ent)
            (setq count (1+ count))
          )
        )
      )
      (setvar "CMDECHO" oldCmdecho)
    )
    (foreach ent elbows
      (if (setq data (dd:GetElbowData ent))
        (progn
          (setq style (car data)
                vanes (nth 4 data))
          (cond
            ((= opt "VANES")
              (setq style "SQUARE"
                    vanes T))
            ((= opt "NOVANES")
              (setq vanes nil))
            ((= opt "RADIUS")
              (setq style "RADIUS"
                    vanes nil))
            ((= opt "SQUARE")
              (setq style "SQUARE"))
          )
          (setq newEnt (dd:RebuildElbow ent style vanes))
          (if newEnt (setq count (1+ count)))
        )
      )
    )
  )
  count
)

(defun c:DDSTYLE (/ opt current)
  (setq current (if dd:elbowStyle (strcase dd:elbowStyle) "AUTO"))
  (initget "Auto Radius Square")
  (setq opt
    (getkword
      (strcat "\nDD elbow style [Auto/Radius/Square] <" current ">: ")
    )
  )
  (if opt
    (setq dd:elbowStyle (strcase opt))
    (setq dd:elbowStyle current)
  )
  (princ (strcat "\nDD elbow style set to " dd:elbowStyle "."))
  (princ)
)

(defun c:DDREDLEN (/ len)
  (setq len
    (getdist
      (strcat
        "\nDD reducer length <"
        (rtos (dd:ReducerLength) 2 2)
        ">: "
      )
    )
  )
  (if len
    (if (> len 0.0)
      (setq *DD_REDUCER_LENGTH* len)
      (princ "\nReducer length must be greater than 0.")
    )
  )
  (princ (strcat "\nDD reducer length is " (rtos (dd:ReducerLength) 2 2) "."))
  (princ)
)

;;; JDaily note: DD is the main duct route builder and the heart of the routine.
(defun c:DD
  ( / actDoc ang1 ang2 ang3 bulges eStyle ptLst enDist
       branchTap fPt joined lEnt lObj lPln oldVars oldWd routeAng routeEnt
       plEnd plStart1 plStart2 prDir
       segLst Start stDist stLst tAng wStart wEnd
       vlaPln cFlg *error*
  )

  (vl-load-com)

  (defun dd:PlineSegmentDataList (plObj / cLst outLst)
    (setq cLst
      (vl-remove-if-not
        '(lambda (x) (member (car x) '(10 40 41 42)))
        (entget plObj)
      )
      outLst '()
    )
    (while cLst
      (if (assoc 40 cLst)
        (progn
          (setq outLst
            (append outLst
              (list
                (list
                  (cdr (assoc 10 cLst))
                  (cdr (assoc 40 cLst))
                  (cdr (assoc 41 cLst))
                  (cdr (assoc 42 cLst))
                )
              )
            )
          )
          (repeat 4 (setq cLst (cdr cLst)))
        )
        (progn
          (setq outLst
            (append outLst
              (list (list (cdr (assoc 10 cLst))))
            )
          )
          (setq cLst nil)
        )
      )
    )
    outLst
  )

  (defun dd:LayersUnlock (/ restLst)
    (setq restLst '())
    (vlax-for lay
      (vla-get-Layers
        (vla-get-ActiveDocument
          (vlax-get-acad-object)
        )
      )
      (setq restLst
        (append restLst
          (list
            (list
              lay
              (vla-get-Lock lay)
              (vla-get-Freeze lay)
            )
          )
        )
      )
      (vla-put-Lock lay :vlax-false)
      (vl-catch-all-apply 'vla-put-Freeze (list lay :vlax-false))
    )
    restLst
  )

  (defun dd:LayersStateRestore (stateList)
    (foreach lay stateList
      (vla-put-Lock (car lay) (cadr lay))
      (vl-catch-all-apply 'vla-put-Freeze (list (car lay) (nth 2 lay)))
    )
    (princ)
  )

  (defun dd:SideCalculate (rad ang)
    (setq ang (- pi ang))
    (*
      (/ (sqrt (- (* 2 (expt rad 2)) (* 2 (expt rad 2) (cos ang))))
         (sin (- pi ang))
      )
      (sin (/ (- pi (- pi ang)) 2))
    )
  )

  (defun dd:DrawPlineSegment (p1 p2 bulge / ent oldPlineWid obj)
    (setq oldPlineWid (getvar "PLINEWID"))
    (setvar "PLINEWID" 0.0)
    (command "_.pline" p1 p2 "")
    (setq ent (entlast))
    (if (and ent bulge (not (equal bulge 0.0 0.00000001)))
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (vla-SetBulge obj 0 bulge)
      )
    )
    (setvar "PLINEWID" oldPlineWid)
    ent
  )

  (defun dd:CornerBulges (a1 a2 / b23 b41 tAng)
    (setq tAng (- a2 a1))
    (if (minusp tAng) (setq tAng (- tAng)))
    (if (and (< 0 tAng) (>= pi tAng))
      (setq b23 (/ (- a2 a1) 4.0)
            b41 (/ (- a1 a2) 4.0))
      (progn
        (if (< a1 a2)
          (setq a1 (+ a1 pi)
                a2 (- a2 pi))
          (setq a1 (- a1 pi)
                a2 (+ a2 pi))
        )
        (setq b23 (/ (- a2 a1) 4.0)
              b41 (/ (- a1 a2) 4.0))
      )
    )
    (list 0.0 b23 0.0 b41)
  )

  (defun dd:DrawQuadPolylines (pts bulgeLst / p1 p2 p3 p4)
    (if (= 4 (length pts))
      (progn
        (setq p1 (nth 0 pts)
              p2 (nth 1 pts)
              p3 (nth 2 pts)
              p4 (nth 3 pts))
        (dd:DrawPlineSegment p1 p2 (nth 0 bulgeLst))
        (dd:DrawPlineSegment p2 p3 (nth 1 bulgeLst))
        (dd:DrawPlineSegment p3 p4 (nth 2 bulgeLst))
        (dd:DrawPlineSegment p4 p1 (nth 3 bulgeLst))
      )
    )
  )

  (defun dd:DrawDuctEdges (p1 p2 width / half perp a b c d)
    (setq half (/ width 2.0)
          perp (+ (/ pi 2.0) (angle p1 p2))
          a (polar p1 perp half)
          b (polar p2 perp half)
          c (polar p2 (+ pi perp) half)
          d (polar p1 (+ pi perp) half))
    (dd:DrawPlineSegment a b 0.0)
    (dd:DrawPlineSegment d c 0.0)
  )

  (defun dd:DrawReducerAndRun (p1 p2 w1 w2 / len redLen redEnd perp pts)
    (setq len (distance p1 p2))
    (if (> len 0.000001)
      (progn
        (setq redLen (dd:Min2 (dd:ReducerLength) len)
              redEnd (polar p1 (angle p1 p2) redLen)
              perp (+ (/ pi 2.0) (angle p1 p2))
              pts
                (list
                  (polar p1 perp (/ w1 2.0))
                  (polar p1 (+ pi perp) (/ w1 2.0))
                  (polar redEnd (+ pi perp) (/ w2 2.0))
                  (polar redEnd perp (/ w2 2.0))
                ))
        (dd:CreateReducerBlock pts w1 w2 (getvar "CLAYER") "BOTH" nil)
        (if (> (distance redEnd p2) 0.000001)
          (dd:DrawDuctEdges redEnd p2 w2)
        )
      )
    )
  )

  (defun dd:BodyFunction ()
    (if (not (equal lObj (entlast)))
      (progn
        (setq lEnt (entlast)
              stLst (dd:LayersUnlock)
              segLst (dd:PlineSegmentDataList lEnt)
              vlaPln (vlax-ename->vla-object lEnt)
        )
        (setvar "OSMODE" 0)
        (setvar "CMDECHO" 0)

        (while (/= 1 (length segLst))
          (setq stDist (vlax-curve-getDistAtPoint vlaPln (caar segLst))
                enDist (vlax-curve-getDistAtPoint vlaPln (caadr segLst))
          )

          (if (< 2 (length segLst))
            (setq ang1 (+ (/ pi 2) (angle (caar segLst) (caadr segLst)))
                  ang2 (+ (/ pi 2) (angle (caadr segLst) (car (nth 2 segLst)))))
          )

          (if (or (not Start) prDir)
            (setq plStart1 (vlax-curve-getPointAtDist vlaPln stDist)
                  Start T)
            (setq plStart1
              (vlax-curve-getPointAtDist vlaPln
                (+ stDist (dd:SideCalculate (cadar segLst) ang3))
              )
            )
          )

          (if (and ang1 ang2)
            (progn
              (if (> ang1 ang2)
                (setq ang3 (- ang1 ang2))
                (setq ang3 (- ang2 ang1))
              )
              (setq ang3 (- pi ang3) tAng ang3)
              (if (minusp ang3) (setq ang3 (- ang3)))
            )
          )

          (if (or (equal ang1 ang2 0.000001) (= 2 (length segLst)))
            (setq plEnd (vlax-curve-getPointAtDist vlaPln enDist)
                  prDir T)
            (setq plEnd
              (vlax-curve-getPointAtDist vlaPln
                (- enDist (dd:SideCalculate (cadar segLst) ang3))
              )
              prDir nil)
          )

          (if (< 2 (length segLst))
            (setq plStart2
              (vlax-curve-getPointAtDist vlaPln
                (+ enDist (dd:SideCalculate (cadar segLst) ang3))
              )
            )
          )

          (if (< 2 (length segLst))
            (if (= (cadar segLst) (nth 2 (car segLst)))
              (setq ptLst
                (mapcar '(lambda (x) (trans x 0 1))
                  (list
                    (polar plEnd ang1 (/ (cadar segLst) 2))
                    (polar plEnd (+ pi ang1) (/ (cadar segLst) 2))
                    (polar plStart2 (+ pi ang2) (/ (cadar segLst) 2))
                    (polar plStart2 ang2 (/ (cadar segLst) 2))
                  )
                )
              )
              (setq ptLst
                (mapcar '(lambda (x) (trans x 0 1))
                  (list
                    (polar plStart1 ang1 (/ (cadar segLst) 2))
                    (polar plStart1 (+ pi ang1) (/ (cadar segLst) 2))
                    (polar (caadr segLst) (+ pi ang2) (/ (nth 2 (car segLst)) 2))
                    (polar (caadr segLst) ang2 (/ (nth 2 (car segLst)) 2))
                  )
                )
              )
            )
          )

          (setq plStart1 (trans plStart1 0 1)
                plEnd    (trans plEnd 0 1)
          )
          (if plStart2 (setq plStart2 (trans plStart2 0 1)))

          (setq wStart (cadar segLst)
                wEnd (nth 2 (car segLst)))

          (if (and (< 2 (length segLst))
                   (not (equal ang1 ang2 0.000001)))
            (progn
              (setq eStyle (dd:ResolveElbowStyle ang1 ang2))
              (dd:CreateElbowBlock
                ptLst
                eStyle
                (- ang1 (/ pi 2.0))
                (- ang2 (/ pi 2.0))
                (getvar "CLAYER")
                nil
              )
            )
          )

          (if (equal wStart wEnd 0.000001)
            (dd:DrawDuctEdges plStart1 plEnd wStart)
            (if (or (= 2 (length segLst))
                    (equal ang1 ang2 0.000001))
              (dd:DrawReducerAndRun plStart1 plEnd wStart wEnd)
            )
          )

          (setq segLst (cdr segLst))
        )

        (command "_.erase" lEnt "")
        (dd:LayersStateRestore stLst)
      )
    )
  )

  (defun *error* (msg)
    (dd:BodyFunction)
    (setq *DD_BRANCH_TAP* nil)
    (if oldVars
      (mapcar 'setvar
        '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE")
        oldVars
      )
    )
    (if actDoc (vla-EndUndoMark actDoc))
    (princ)
  )

  (if (not dpipepWd) (setq dpipepWd 1.0))
  (setq oldWd   dpipepWd
        oldVars (mapcar 'getvar '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE"))
        branchTap *DD_BRANCH_TAP*
  )
  (if (entlast) (setq lObj (entlast)))

  (vla-StartUndoMark
    (setq actDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  )

  (if *DD_START_POINT*
    (setq fPt *DD_START_POINT*
          cFlg T
          *DD_START_POINT* nil)
    (progn
      (initget 128)
      (while (not cFlg)
        (setq fPt
          (getpoint
            (strcat "\nSpecify start point or width <"
                    (rtos dpipepWd) ">: ")
          )
        )
        (cond
          ((= 'LIST (type fPt)) (setq cFlg T))
          ((= 'REAL (type (distof fPt))) (setq dpipepWd (distof fPt)))
          (T (princ "\nInvalid option keyword! "))
        )
      )
    )
  )

  (mapcar 'setvar '("FILLMODE" "PLINEWID" "CMDECHO")
                    (list 0 dpipepWd 0))
  (princ "\nDraw duct route, then press Enter to finalize.")
  (command "_.pline" fPt)
  (setvar "CMDECHO" 1)

  (while (= 1 (getvar "CMDACTIVE"))
    (command pause)
  )

  (setq routeEnt (entlast))
  (if branchTap
    (setq routeAng (dd:FirstSegmentAngle routeEnt))
  )
  (dd:BodyFunction)
  (if (and branchTap routeAng)
    (progn
      (dd:BreakMainEdgesForBranch branchTap routeAng)
      (dd:DrawBranchCollarLineAtAngle branchTap routeAng)
    )
  )
  (setq *DD_BRANCH_TAP* nil)
  (setq joined (dd:JoinCollinearPlineEdges (dd:EntitiesAfter lObj) 0.01))
  (if (> joined 0)
    (princ (strcat "\nJoined " (itoa joined) " straight duct edge segment(s)."))
  )
  (vla-EndUndoMark actDoc)

  (mapcar 'setvar
          '("FILLMODE" "PLINEWID" "CMDECHO" "OSMODE")
          oldVars)

  (princ)
)

(defun c:DDB (/ sizeStr size sel edgeEnt pickPt edgePt axisAng opposite center layer)
  (vl-load-com)
  (if (not dpipepWd) (setq dpipepWd 8.0))
  (setq sizeStr
    (getstring
      T
      (strcat "\nBranch duct size <" (rtos dpipepWd 2 2) "\">: ")
    )
  )
  (setq size
    (cond
      ((= "" sizeStr) dpipepWd)
      ((distof sizeStr) (distof sizeStr))
    )
  )
  (cond
    ((not size)
      (princ "\nInvalid branch duct size."))
    ((<= size 0.0)
      (princ "\nBranch duct size must be greater than 0."))
    (T
      (setq sel (entsel "\nPick main duct edge at branch/takeoff location: "))
      (cond
        ((not sel)
          (princ "\nNothing selected."))
        ((not (dd:CurveEntityP (car sel)))
          (princ "\nSelect a duct edge line or polyline."))
        (T
          (setq edgeEnt (car sel)
                pickPt (cadr sel)
                edgePt (dd:CurveClosestPoint edgeEnt pickPt)
                axisAng (if edgePt (dd:CurveTangentAngle edgeEnt edgePt)))
          (if (and edgePt axisAng)
            (progn
              (setq opposite (dd:FindOppositeDuctEdge edgeEnt edgePt axisAng))
              (if opposite
                (progn
                  (setq center (dd:MidPoint edgePt (nth 2 opposite))
                        layer (cdr (assoc 8 (entget edgeEnt))))
                  (dd:EntmakeCircle center (/ size 2.0) layer)
                  (setq dpipepWd size
                        *DD_START_POINT* center
                        *DD_BRANCH_TAP* (list center size layer (list edgeEnt (car opposite))))
                  (princ "\nBranch tap placed. Draw branch duct route, then press Enter to finalize.")
                  (c:DD)
                )
                (princ "\nCould not find the opposite parallel duct edge. Try picking closer to a straight duct side.")
              )
            )
            (princ "\nCould not read that duct edge direction.")
          )
        )
      )
    )
  )
  (princ)
)

(defun c:DDV (/ sizeStr size sel edgeEnt pickPt edgePt dirPt axisAng opposite layer result)
  (vl-load-com)
  (if (not dpipepWd) (setq dpipepWd 8.0))
  (setq sizeStr
    (getstring
      T
      (strcat "\nVCD size <" (rtos dpipepWd 2 2) "\">: ")
    )
  )
  (setq size
    (cond
      ((= "" sizeStr) dpipepWd)
      ((distof sizeStr) (distof sizeStr))
    )
  )
  (cond
    ((not size)
      (princ "\nInvalid VCD size."))
    ((<= size 0.0)
      (princ "\nVCD size must be greater than 0."))
    (T
      (setq sel (entsel "\nClick duct edge where VCD starts: "))
      (cond
        ((not sel)
          (princ "\nNothing selected."))
        ((not (dd:CurveEntityP (car sel)))
          (princ "\nSelect a duct edge line or polyline."))
        (T
          (setq edgeEnt (car sel)
                pickPt (cadr sel)
                edgePt (dd:CurveClosestPoint edgeEnt pickPt))
          (if edgePt
            (progn
              (setq dirPt (getpoint edgePt "\nPick duct run direction: "))
              (if (and dirPt (> (distance edgePt dirPt) 0.000001))
                (progn
                  (setq axisAng (angle edgePt dirPt)
                        opposite (dd:FindOppositeDuctEdge edgeEnt edgePt axisAng)
                        layer (cdr (assoc 8 (entget edgeEnt)))
                        result (dd:DrawVcdSymbol edgePt axisAng size layer opposite)
                        dpipepWd size)
                  (princ
                    (strcat
                      "\nVCD placed. Offset "
                      (rtos (cadr result) 4 4)
                      ", handle extension "
                      (rtos (caddr result) 4 4)
                      "."
                    )
                  )
                )
                (princ "\nDirection point must be different from the edge point.")
              )
            )
            (princ "\nCould not read that duct edge.")
          )
        )
      )
    )
  )
  (princ)
)

(defun c:DDE (/ ss elbows opt current count)
  (vl-load-com)
  (princ "\nSelect DD elbow block(s) to edit: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (setq elbows (dd:ElbowsFromSelection ss))
  (cond
    ((not ss)
      (princ "\nNothing selected."))
    ((not elbows)
      (princ "\nNo DD elbow blocks selected."))
    (T
      (setq current (dd:FirstElbowStyle elbows))
      (initget "Radius Square Vanes NoVanes Explode")
      (setq opt
        (getkword
          (strcat "\nElbow style [Radius/Square/Vanes/NoVanes/Explode] <" current ">: ")
        )
      )
      (cond
        ((not opt)
          (princ "\nNo change."))
        (T
          (setq count (dd:ApplyElbowEdit elbows opt))
          (cond
            ((= "EXPLODE" (strcase opt))
              (princ (strcat "\nExploded " (itoa count) " DD elbow block(s) to polylines.")))
            ((= "VANES" (strcase opt))
              (princ (strcat "\nAdded turning vanes to " (itoa count) " DD elbow block(s).")))
            ((= "NOVANES" (strcase opt))
              (princ (strcat "\nRemoved turning vanes from " (itoa count) " DD elbow block(s).")))
            (T
              (princ (strcat "\nChanged " (itoa count) " DD elbow block(s) to " (strcase opt) ".")))
          ))
      )
    )
  )
  (princ)
)

(defun c:DDHEAL (/ ent tol ss count)
  (vl-load-com)
  (setq ent (car (entsel "\nSelect DD reducer block to reconnect ductwork: ")))
  (cond
    ((not ent)
      (princ "\nNothing selected."))
    ((not (dd:GetReducerData ent))
      (princ "\nThat is not a DD reducer block."))
    (T
      (setq tol (dd:PromptHealTolerance)
            ss (dd:PromptHealSelection)
            count (dd:HealReducerDucts ent tol ss))
      (princ (strcat "\nMoved " (itoa count) " duct object(s) to reducer."))
    )
  )
  (princ)
)

(defun c:DDJOIN (/ ss ents count)
  (vl-load-com)
  (princ "\nSelect straight duct edge polylines to join, or press Enter for all: ")
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (not ss)
    (setq ss (ssget "_X" '((0 . "LWPOLYLINE"))))
  )
  (setq ents (dd:SsToList ss)
        count (dd:JoinCollinearPlineEdges ents 0.01))
  (princ (strcat "\nJoined " (itoa count) " straight duct edge segment(s)."))
  (princ)
)

(defun c:DDR (/ ent data opt pick style newEnt tol ss count)
  (vl-load-com)
  (setq ent (car (entsel "\nSelect DD reducer block to edit: ")))
  (cond
    ((not ent)
      (princ "\nNothing selected."))
    ((not (setq data (dd:GetReducerData ent)))
      (princ "\nThat is not a DD reducer block."))
    (T
      (initget "Both Single Flip Reconnect Explode")
      (setq opt
        (getkword
          (strcat "\nReducer style [Both/Single/Flip/Reconnect/Explode] <" (nth 3 data) ">: ")
        )
      )
      (cond
        ((not opt)
          (princ "\nNo change."))
        ((= "EXPLODE" (strcase opt))
          (command "_.explode" ent)
          (princ "\nDD reducer exploded to polylines."))
        ((= "BOTH" (strcase opt))
          (setq newEnt (dd:RebuildReducer ent "BOTH"))
          (if newEnt
            (princ "\nDD reducer changed to both-side diagonal.")
            (princ "\nUnable to rebuild DD reducer.")
          ))
        ((= "FLIP" (strcase opt))
          (setq style
            (cond
              ((= "SIDEA" (nth 3 data)) "SIDEB")
              ((= "SIDEB" (nth 3 data)) "SIDEA")
              (T "SIDEB")))
          (setq newEnt (dd:RebuildReducer ent style))
          (if newEnt
            (princ "\nDD reducer single diagonal flipped.")
            (princ "\nUnable to rebuild DD reducer.")
          ))
        ((= "RECONNECT" (strcase opt))
          (setq tol (dd:PromptHealTolerance)
                ss (dd:PromptHealSelection)
                count (dd:HealReducerDucts ent tol ss))
          (princ (strcat "\nMoved " (itoa count) " duct object(s) to reducer.")))
        ((= "SINGLE" (strcase opt))
          (setq pick (getpoint "\nPick the side of reducer that should be diagonal: ")
                style (dd:ReducerStyleFromDiagonalPick (nth 2 data) pick)
                newEnt (dd:RebuildReducer ent style))
          (if newEnt
            (princ "\nDD reducer changed to single diagonal.")
            (princ "\nUnable to rebuild DD reducer.")
          ))
      )
    )
  )
  (princ)
)

;;; Note: finalize turns edited blocks back into plain polylines for downstream use.
(defun c:DDFINAL (/ ss idx ent count oldCmdecho)
  (vl-load-com)
  (princ "\nSelect DD elbow/reducer blocks to finalize, or press Enter for all DD blocks: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss)
    (setq ss (ssget "_X" '((0 . "INSERT"))))
  )
  (setq count 0
        oldCmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (if ss
    (progn
      (setq idx 0)
      (while (< idx (sslength ss))
        (setq ent (ssname ss idx))
        (if (or (dd:GetElbowData ent)
                (dd:GetReducerData ent))
          (progn
            (command "_.explode" ent)
            (setq count (1+ count))
          )
        )
        (setq idx (1+ idx))
      )
    )
  )
  (setvar "CMDECHO" oldCmdecho)
  (princ (strcat "\nFinalized " (itoa count) " DD block(s) to polylines."))
  (princ)
)

(princ "\nDD duct draw loaded. Commands: DD, DDB, DDV, DDSTYLE, DDREDLEN, DDE, DDR, DDHEAL, DDJOIN, DDFINAL")
(princ)
