function [y alpha] = sv_sim(theta,T)

   const = theta(1);
   phi = theta(2);
   tau2 = theta(3);

   alpha = zeros(T,1);
   y = zeros(T,1);

   eta = normrnd(0,sqrt(tau2),T,1);
   z   = normrnd(0,1,T,1);
   nu  = unifrnd(0,1,T,1);

   alpha(1) = const;
   y(1) = z(1) * exp( alpha(1)/2 );
   for t=2:T
      alpha(t) = const + phi*(alpha(t-1)) + eta(t);
      y(t) = z(t)  * exp( alpha(t)/2 );
   end

