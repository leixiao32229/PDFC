function [x,fval,exitflag,output,lambda,grad,hessian] = logisticprice_demand(x0,d,k,lambda,cost,tol,Aineq,bineq,lb,ub)
%% This is an auto generated MATLAB file from Optimization Tool.

%% Start with the default options
options = optimoptions('fmincon');
%% Modify options setting
options = optimoptions(options,'Display', 'off');
[x,fval,exitflag,output,lambda,grad,hessian] = ...
fmincon(@(x)-d(1)*(x(1)-cost)*k(1)*exp(-lambda(1)*x(1))/(1+k(1)*exp(-lambda(1)*x(1)))-d(2)*(x(2)-cost)*k(2)*exp(-lambda(2)*x(2))/(1+k(2)*exp(-lambda(2)*x(2))),x0,Aineq,bineq,[],[],lb,ub,@(x)demandlogistic(x,lambda,k,tol),options);
