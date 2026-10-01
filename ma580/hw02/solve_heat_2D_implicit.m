function [U_array ti Xi Yi] = solve_heat_2D_implicit
% solves the following PDE using second order accurate 
% finite-differences in space and implicit Euler
% in time:
%
%   u_t + Laplacian u = 0         in Omega x [0, T]
%   u(x, 0)           = u0(x)     in Omega
%   
%   with homogeneous Dirichlet boundary conditions.
%   the domain Omega = (0, 1) x (0, 1).
%  
%   output: 
%      U_array ---- a cell array with U_array{i} = U(x, t_i)
%      ti      ---- vector of time steps      
%      Xi, Yi  ---- mesh grid of spatial points 
%                   (useful for plotting stuff).

%
% PDE parameters
%
% initial condition 
u0  = @(x,y)( ...
              ((x-.125).^2 + (y-.125).^2 <= .01) * 10 + ... 
              ((x-.5).^2 + (y-.5).^2 <= .01) * 5); 
% diffusion coefficient
kappa = .001;      

%
% spatial discretization
% 
nx = 2^7;
h = 1/nx;
xi = [0 : h : 1]'; yi = xi;
[Xi Yi] = meshgrid(xi,xi);

% the spatial discretization matrix 
A = get_discrete_laplacian(nx);
A = kappa * (1/h^2) * A; 

% evaluate initial state
U0 = u0(Xi,Yi);
U0 = U0(2:end-1, 2:end-1);    % need y0 on interior nodes 

%
% time discretization
%
t0 = 0;
Tf = 10;
nt = 500+1;
dt = Tf / (nt-1); 
ti = [t0 : dt : Tf];

%
% time-stepping
%
U = zeros((nx-1)^2,nt+1);
U(:,1) = U0(:);
I = speye((nx-1)^2);

fprintf('time integration in progress ...\n');
tic; 
G = I + dt * A;
for n = 1 : nt
   % show progress 
   if mod(ti(n), 1) == 0
      fprintf('t = %4.2f\n', ti(n));
   end

   % euler step
   U(:,n+1) = G \ U(:,n);
end
compute_time = toc;
fprintf('time integration complete in %g seconds\n', compute_time);

% store the solution in U_array
U_mat = zeros(nx+1);
for i = 1 : nt
   U_mat(2:end-1,2:end-1) = reshape(U(:, i), nx-1,nx-1);
   U_array{i} = U_mat;
end


%%%%%%%%%%%%%%%% subfunctions %%%%%%%%%%%%%%%%%% 
%
% Computes discretization of Lu = -Laplacian u 
%
function A = get_discrete_laplacian(nx)
h = 1/nx;
m = nx-1;
I = speye(m);
e = ones(m,1);
T = spdiags([-e 4*e -e],[-1 0 1],m,m);
S = spdiags([-e -e],[-1 1],m,m);
A = kron(I,T) + kron(S,I);

