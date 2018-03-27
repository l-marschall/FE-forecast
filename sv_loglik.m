function [loglik alpha_up_pr] = sv_loglik(theta, y, eta_sim, u_sim, alpha_up, alpha_wt)

   T = length(y);
   P = length(alpha_up);

   const = theta(1);
   phi = theta(2);j
   tau2 = theta(3);

   alpha_up_pr = zeros(T, 4);

   loglik = 0;
   for t = 1:T
      alpha_pr = const + phi.* alpha_up + sqrt(tau2).*eta_sim(:, t);

      lik = normpdf( y(t)*ones(P,1) , zeros(P,1) , exp(alpha_pr./2) );

      if isfinite( log(mean(lik)) )
         loglik = loglik - log( mean( lik ) );
      else
         fprintf('problem at %d: %s\n',t,num2str(theta))alpha_up_quant
         loglik = inf;
         return;
      end

      % update
      alpha_wt = lik;
      alpha_up = csir(alpha_pr,alpha_wt,u_sim(:,t));

      % quant
      alpha_up_pr(t,1) = mean( alpha_up );
      alpha_up_pr(t,2) = mean( alpha_pr );
      alpha_up_pr(t,3:4) = quantile( alpha_pr ,[0.05 0.95]);
   end

   loglik = loglik/T;
