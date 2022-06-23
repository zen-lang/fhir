WITH
tmp AS
(
	SELECT rxcui, tty, str
       , CASE
           WHEN tty='SCD' THEN 110
           WHEN tty='SCDG' THEN 120
           WHEN tty='SCDF' THEN 130
           WHEN tty='SCDC' THEN 140
           WHEN tty='SBD' THEN 210
           WHEN tty='SBDG' THEN 220
           WHEN tty='SBDF' THEN 230
           WHEN tty='SBDC' THEN 240
           WHEN tty='MIN' THEN 310
           WHEN tty='PIN' THEN 320
           WHEN tty='IN' THEN 330
           WHEN tty='GPCK' THEN 410
           WHEN tty='BPCK' THEN 420
           WHEN tty='PSN' THEN 510
           WHEN tty='SY' THEN 520
           WHEN tty='TMSY' THEN 530
           WHEN tty='BN' THEN 610
           WHEN tty='DF' THEN 710
           WHEN tty='ET' THEN 720
           WHEN tty='DFG' THEN 730
           ELSE NULL
         END AS order_cmp
  FROM rxnconso
  WHERE sab='RXNORM'
  ORDER BY rxcui, order_cmp
)
SELECT rxcui, tty, str
FROM tmp
GROUP BY rxcui
HAVING MIN(order_cmp);
