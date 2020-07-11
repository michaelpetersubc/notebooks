#warning('off','all');
#clear
#output_precision(9)
pkg install -forge symbolic
pkg load symbolic
#import sympy.core.compatibility
syms r v_min v_max t 
syms s v p F;

%---------------INPUT WINDOW-------------------%
n = 2;            % should > 1
alpha = .8;      % should in (0,1)
#F(v) = v^(.5);   % should monotonically increasing; F(0) = 0; F(1) = 1
z=200.0
F(v) = ((.5)*(sqrt(z*v^4 + 1.) - 1.)/v^2)/(.5*(sqrt(z*1^4 + 1.) - 1.))
%---------------INPUT WINDOW-------------------%

f = diff(F);
pi(p) = (1-F(p))*p;
phi(v) = v - (1-F(v))/f;
chi(v_min, v_max) = (((1-alpha)*F(v_max)+alpha)^n-((1-alpha)*F(v_min))^n) / (n*(1-alpha)*(F(v_max)-F(v_min))+alpha);

eq1 = (alpha * (pi(t)-phi(v_max)) == (1-alpha)*((v_min-t)*(phi(v_max)-phi(v_min))*f(v_min)+(F(v_max)-F(v_min))*phi(v_max)-(pi(v_min)-pi(v_max))));
eq2 = (-alpha*diff(pi(t),t) == (1-alpha)*(phi(v_max)-phi(v_min))*f(v_min));
eq3 = (-phi(r)*f(r) == (phi(v_max)-phi(v_min))*f(v_min));
eq4 = ((1-alpha)^(n-1) * int(F(s)^(n-1),s,r,v_min) == chi(v_min,v_max)*(v_min-t));

%[r_s, t_s, v_min_s, v_max_s] = vpasolve([eq1 eq2 eq3 eq4],[r t v_min v_max],[[0,1]; [0,1]; [0,1]; [0,1]]);
out = vpasolve([eq1 eq2 eq3 eq4],[r t v_min v_max],[0.5; 0.5; 0.5; 0.5]);
r_s = double(out(1));
t_s = double(out(2));
v_min_s = double(out(3));
v_max_s = double(out(4));
disp([r_s, t_s, v_min_s, v_max_s]);

if (v_max_s >= v_min_s && v_min_s >= t_s && t_s >= r_s)
    fprintf('Inequality condition is met\n');
else
    fprintf('WARNING: Inequality condition is not met\n');
end

