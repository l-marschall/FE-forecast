function alpha_up = sir(alpha_pr, alpha_wt, u)

   % u needs to be sorted!!!!

   % save DEBUG

   P = length(alpha_pr);

   % updated vector
   alpha_up = zeros(P,1);

   % sort weights and particles
   % :-( this slows down things.... :-(
   alpha_wt = alpha_wt ./ sum( alpha_wt );
   [alpha_pr alpha_idx]= sort(alpha_pr);
   alpha_wt = alpha_wt( alpha_idx );
   alpha_cwt = [0; cumsum(alpha_wt)];

   j=1;
   for i=1:P
      while alpha_cwt(i) < u(j) && u(j) <= alpha_cwt(i+1)
         alpha_up(j) = alpha_pr(i);
         if j<P
            j = j+1;
 	 else
            break;
	 end
     end
   end
