function [loglik theta_sml theta_se alpha_up_quant] = sv_fit(y, theta, P, estimate)

   T = length(y);

   alpha_up_0 = normrnd(0,1,P,1);
   alpha_wt_0 = ones(P,1)./P;
   eta_sim    = normrnd(0,1,P,T);
   u_sim      = unifrnd(0,1,P,T);

   for t=1:T
      u_sim(:,t) = sort( u_sim(:,t) );
   end

   if estimate
      options = optimset('fmincon');
      options = optimset(options , 'Display'     , 'iter');
      options = optimset(options , 'Diagnostics' , 'off');
      options = optimset(options , 'TolCon',1e-10);
      options = optimset(options , 'Algorithm','active-set');
      options = optimset(options , 'TolFun',1e-12);

      lb = zeros(size(theta))+0.001;
      ub = ones(size(theta))-2*options.TolCon;

      fprintf('estimating...')
      [theta_sml,loglik,EXITFLAG,OUTPUT,LAMBDA,GRAD,HESSIAN] = fmincon('sv_loglik', ...
         theta,[],[],[],[],lb,ub,[],options,y,eta_sim,u_sim,alpha_up_0,alpha_wt_0);
      theta_se = diag(sqrt(inv(HESSIAN)));
      fprintf('... done!\n')
   else
     theta_sml =[];
     theta_se  =[];
   end

   [ll alpha_up_quant] = sv_loglik(theta, y, eta_sim, u_sim, alpha_up_0, alpha_wt_0);

   loglik = -ll;
