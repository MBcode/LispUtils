;test of distributing files among nodes, to be redone in C&maybe Python;  mike.bobak@gmail.com
;(load "util_mb") ;this is another test of the use of my util_mb.lisp but only uses a few fncs
;USE:  sbcl --eval '(progn (load "util_mb") (load "tbl.cl") (setf *dbg* nil) (tst))'
(defvar *dbg* t)

(defun split2n (txt2)
  "split txt-pair, change str->num on 2nd"
  (let ((tl (split txt2)))
    (list (first tl) (numstr (second tl)) 
          (len (second tl)) ;quickly see the size of a number
          )))

(defun txtfile2alst (fn)
  "split 2col fn.txt"
  (rest (mapcar #'split2n (list-lines (str-cat fn ".txt")))))

(defun txtfile2srt-alst (fn)
  (sort (txtfile2alst fn) #'> :key #'second))

;local accessors
(defun name (pr)
  "a-lst has name in 1st position"
  (first pr))
(defun size (pr)
  "a-lst has size in 2nd position"
  (second pr))

(defun assign-f2n (f1 n1)
  "file to node, alter node size" ;should remove file so can't reassign
  ;if (< (size f1) (size n1))  ;decf node size
  (rplaca (rest n1) (- (size n1) (size f1)))
  (cons (name f1) (name n1))) ;ret: SizedNameAssignment pair

(defun f-per-n (sf sn)
  "file per node"
  ;(mapcar #'(lambda (f n) (list (name f) (name n))) sf sn)
  (mapcar #'(lambda (f n) (assign-f2n f n) (list (name f) (name n))) sf sn)
  ) ;get rid of &easy test, as the more general should handle it


(defun distribute2 (sf2 sn2) 
  "all in one distribute helper, makes as many passes over the nodes as needed"
 (let ((out sf2)  ;;ran-out(not yet placed in a pass)sized-Files ;set to sf2  for test below
       (fstp 0)) ;files set this pass
   (labels ((adapt-f2n-pass (sf sn) ;flet doesn't allow rec calls,right away
     (let* ((f1 (first sf)) ;try to match the 2 largest 1st
            (n1 (first sn))); from the sorted lists of files&nodes
       (cond ;make >1 pass now, so it now just what wasn't placed on this pass
         ((null f1) (setf out nil) ;no files w/o nodes
            nil)   
         ((null n1)        ;went through all the nodes 
            (if (eq fstp 0)  ;This will catch a pass that can't assign any files, w/the asked Warning
              (progn (format t "~%This Distribution Ran OUT of Nodes:~%~a ~a" sf fstp) (setf out nil))
              (progn (when *dbg* (format t "~%this distrib-PASS ran out of nodes:~%~a ~a" sf fstp))
                     (setf out sf)))
            nil) ;test data only has files left after run/pass, not in the end
        (t    
          (if (<= (size f1) (size n1)) ;maybe also pop/ (remove f1 sf)
            (progn (incf fstp) 
             (cons (assign-f2n f1 n1) (adapt-f2n-pass (rest sf) sn)) )
            (adapt-f2n-pass sf (rest sn)))))))) ;try other nodes 
     ;go for a pass until either out is nil (=run out of files)
    (if (full out)   ;have some but not all assignments yet
     (let ((sna (adapt-f2n-pass out sn2))) ;generated node assignments for this pass
       (when *dbg*
        (format t "~%cur-snAssigned:~a" sna) ;tmp 
        (format t "~%cur-out_of-pass:~a" out)) ;tmp
      (if (> fstp 0)
        (cons sna (distribute2 out sn2))  ;so send undistributed files to another distribution pass
        sna))
     nil))))
    ;need a way to know that not even 1 of the out=sf files could be put in any of the nodes ;fstp

 
(defun sum-2nd (l) (reduce #'+ (mapcar #'second l)))
(defun pct (a b) (/ (- b a) (* 0.01 b)))

(defun distribute (f-fn n-fn)
  "get input &start doling out the files"
  (let* ((sf (txtfile2srt-alst f-fn))
         (sn (txtfile2srt-alst n-fn))
         (lf (len sf))
         (ln (len sn))
         (tf (sum-2nd sf))
         (tn (sum-2nd sn))
         (easy (>= ln lf)))
    (when *dbg*
     (format t "~%~a ~d:file-sz than ~d:node-sz so ~a~%" 
            (pct tf tn) tf tn (if (> tn tf) 'ok 'bad))
     (format t "~%~a ~d:files than ~d:nodes so ~a~%"
            (if easy 'fewer 'more) lf ln (if easy 'easy 'gather)))
    (if easy (f-per-n sf sn) ;could get rid of this case, but ok to leave 
      (let ((sna (flat1 (distribute2 sf sn))))
       (mapcar #'(lambda (fn-pr) (format t "~%~a ~a" (first fn-pr) (rest fn-pr))) sna)
       sna))))

(defun tst () 
  "try it out"
   (distribute "files" "nodes"))
 
;can easily (trace distribute2) to see the Size(of the)NodeAssignments, drop
;=had the start of the C version in the last commit, &have a Python started offline ;lsp-like
;USER(1): (tst)
;
;11.175601 433984592140:file-sz than 488587138990:node-sz so OK
;
;MORE 24:files than 10:nodes so GATHER
;
;this distrib-PASS ran out of nodes:
;((file18 6609806629 10) (file11 6348867697 10) (file15 5942107928 10)
; (file9 4495356117 10) (file10 3118866364 10) (file17 2424678728 10)
; (file14 1293428979 10) (file8 170858581 9)) 16
;cur-snAssigned:((file16 . node5) (file6 . node5) (file21 . node0)
;                (file3 . node0) (file0 . node6) (file1 . node6)
;                (file13 . node9) (file4 . node9) (file20 . node7)
;                (file7 . node7) (file23 . node8) (file19 . node8)
;                (file2 . node4) (file22 . node4) (file12 . node1)
;                (file5 . node3))
;cur-snAssigned:((file18 . node5) (file11 . node0) (file15 . node6)
;                (file9 . node6) (file10 . node9) (file17 . node9)
;                (file14 . node9) (file8 . node9))
;==those not assigned on 1st pass got assigned in the 2nd  
;;;use next line to just get file-assignments:
;sbcl --noinform --eval '(progn (load "util_mb") (load "tbl.cl") (setf *dbg* nil) (tst))'
;file16 node5
;file6 node5
;file21 node0
;file3 node0
;file0 node6
;file1 node6
;file13 node9
;file4 node9
;file20 node7
;file7 node7
;file23 node8
;file19 node8
;file2 node4
;file22 node4
;file12 node1
;file5 node3
;file18 node5
;file11 node0
;file15 node6
;file9 node6
;file10 node9
;file17 node9
;file14 node9
;file8 node9 
//a bit in C w/o looking much up yet  //only somewhat similar pass in C at lisp version;bobak
//quick look@stella to dump to java/c++ vs recoding
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
/* Define an array of name&sized entities to sort. */
//////struct sn { const char *name; int size}; //add so files can pnt2nodes
//typedef struct sn { const char *name; int size;  struct sn *node;} //ptr for node
// files[99],nodes[99];
	//need to get so has ptr to self, might subclass a version w/that added
//alloc later //check if can reuse struct name&global var
//struct sn files[99]; struct sn nodes[99];

//int read_sn (FILE *fp, struct sn *ns) 
//{ fscanf(fp,"%c %d", ns->name , ns->size); } 
int read_sn (FILE *fp, int **ns, int i) 
{ 
//	char name[20];
//	fscanf(fp,"%c %d", name , ns[i]); 
	char type[5];  //could readline & sscanf
	fscanf(fp,"%4c%d %d\n", type , &ns[i][0],&ns[i][1]); 
	printf("\n%s %d,%d", type, ns[i][0],ns[i][1]);
} 

//int read_sn_file(FILE *fp,struct sn **sa)
int read_sn_file(FILE *fp,int **sn)
{   int i=0;
	while (read_sn(fp,sn,i++)!=EOF); 
i;}

//int sn_cmp (const struct sn *c1, const struct sn *c2) { return (c1->size > c2->size); } 
int sn_cmp (int *c1, int *c2) { return (c1[1] > c2[1]); } 
//void print2sn (struct sn *c1, struct sn *c2) { printf ("%s, the %s\n", c1->name, c2->name); } 
void print_f2n(int *c2)
{
	printf("\nfile%d node%d", c2[0],c2[1]);
}

int  files[99][2],nodes[49][2];
int f2n[99][2]; //assingments, 2:so could go w/str instead, of assume index was of file#
int nf=0,nn=0;

int assign_f2n(int fi,int ni)
{
	f2n[fi][0]=files[fi][0];
	f2n[fi][1]=nodes[ni][0];
} //now should pop off/mark some way as unavailable ;should have a queqe/or?

//void print_sn (struct sn *c3) { print2sn(c3, c3->node); }
//struct sn **gather_adapt_f2n(struct sn **sf, struct sn **sn)
//{ if(sf[0]=='\0' || sn[0]=='\0') '\0'; 
//   //could skip rec mk list &iterate over twice here
	   //use sn_cmp ..  }
int *adapt_f2n_pass(int **sf, int **sn)
{
	int fi=0,ni=0;
	for(ni=0;ni<nn;ni++) 
			for(fi=0;fi<nf;fi++) if(sn_cmp(sf[fi],sn[ni])) assign_f2n(fi,ni);
}

int main (int argc, char *argv[])
{
FILE *file_fp, *node_fp;
//typedef struct sn { const char *name; int size;  struct sn *node;} files[99],nodes[99];
//int file_fp, node_fp,i;
int i;
//int  files[99],nodes[49];
//	file_fp=open(argv[1],"r");
//	node_fp=open(argv[2],"r");
	file_fp=(FILE *)open("files.txt","r");
	node_fp=(FILE *)open("nodes.txt","r");
	nf = read_sn_file(file_fp,files);
	nn = read_sn_file(node_fp,nodes);
	printf("\nGot,%d files and %d nodes", nf, nn);
//	qsort(files, nf, sizeof(struct sn), sn_cmp); 
//	qsort(nodes, nn, sizeof(struct sn), sn_cmp); 
//	gather_adapt_f2n(*files, *nodes);
	//need2sort names at same time
	qsort(files, nf, 2*sizeof(int), &sn_cmp); 
	qsort(nodes, nn, 2*sizeof(int), &sn_cmp); 
	adapt_f2n_pass(*files, *nodes);
//	for(i=0; i<nf; i++) print_sn(files[i]);
	for(i=0; i<nf; i++) print_f2n(f2n[i]);
}
#If I did more writing than reading of Python I'd do a nice translation w/it
import csv
def get_ns_file(fn):
    l = []
    fp = open(fn, "r")
    rdr = csv.reader(fp, delimiter=' ')
    for row in rdr:
        print row
        l.append(row)
    for p in l[1:]:
        p[1]=int(p[1])
    fp.close()
    return l[1:]

get_ns_file('files.txt')
get_ns_file('nodes.txt')
#then sort the files &write def distribute 
print 'now sort them'
from operator import itemgetter, attrgetter
fs=sorted(fa,key=itemgetter(1))
ns=sorted(na,key=itemgetter(1))
for i in fs:
    print i
for i in ns:
    print i
#&write def distribute 
