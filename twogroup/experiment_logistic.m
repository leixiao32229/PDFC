lambda1=[1 0.2];
k1=[10 5];
d=[0.5 0.5];
cost=0.5;
x0=[1 1];
x0=logisticprice(x0,d,k1,lambda1,cost,[],[],[0 0],[100 100]);
p0list=[];
p1list=[];
revlist=[];
surlist=[];
%price
for a=0:0.01:0.99
    tol=(1-a)*(x0(2)-x0(1));
    [price, rev]=logisticprice(x0,d,k1,lambda1,cost,[1 -1;-1 1],[tol tol],[0 0],[100 100]);
    rev=-rev;
    sur=d(1)*log(1+k1(1)*exp(-lambda1(1)*price(1)))/lambda1(1)+d(2)*log(1+k1(2)*exp(-lambda1(2)*price(2)))/lambda1(2);
    p0list=[p0list price(1)];
    p1list=[p1list price(2)];
    revlist=[revlist rev];
    surlist=[surlist sur];
end
hplot=plot(0:0.01:0.99,p0list,0:0.01:0.99,p1list);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0:0.01:0.99,revlist,0:0.01:0.99,surlist,0:0.01:0.99,revlist+surlist);
xlabel('\alpha','FontSize',18)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)



%demand
demand0=k1(1)*exp(-lambda1(1)*x0(1))/(1+k1(1)*exp(-lambda1(1)*x0(1)));
demand1=k1(2)*exp(-lambda1(2)*x0(2))/(1+k1(2)*exp(-lambda1(2)*x0(2)));
delta0=abs(demand0-demand1);
p0list2=[];
p1list2=[];
revlist2=[];
surlist2=[];
for a=0:0.01:0.99
    tol=(1-a)*delta0;
    [price, rev]=logisticprice_demand(x0,d,k1,lambda1,cost,tol,[],[],[0 0],[100 100]);
    rev=-rev;
    sur=d(1)*log(1+k1(1)*exp(-lambda1(1)*price(1)))/lambda1(1)+d(2)*log(1+k1(2)*exp(-lambda1(2)*price(2)))/lambda1(2);
    p0list2=[p0list2 price(1)];
    p1list2=[p1list2 price(2)];
    revlist2=[revlist2 rev];
    surlist2=[surlist2 sur];
end
hplot=plot(0:0.01:0.99,p0list2,0:0.01:0.99,p1list2);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0:0.01:0.99,revlist2,0:0.01:0.99,surlist2,0:0.01:0.99,revlist2+surlist2);
xlabel('\alpha','FontSize',18)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)


%surplus
surplus0=log(1+k1(1)*exp(-lambda1(1)*x0(1)))/lambda1(1);
surplus1=log(1+k1(2)*exp(-lambda1(2)*x0(2)))/lambda1(2);
delta0=abs(surplus0-surplus1);
p0list3=[];
p1list3=[];
revlist3=[];
surlist3=[];
for a=0:0.01:0.99
    tol=(1-a)*delta0;
    [price, rev]=logisticprice_surplus(x0,d,k1,lambda1,cost,tol,[],[],[0 0],[100 100]);
    rev=-rev;
    sur=d(1)*log(1+k1(1)*exp(-lambda1(1)*price(1)))/lambda1(1)+d(2)*log(1+k1(2)*exp(-lambda1(2)*price(2)))/lambda1(2);
    p0list3=[p0list3 price(1)];
    p1list3=[p1list3 price(2)];
    revlist3=[revlist3 rev];
    surlist3=[surlist3 sur];
end
hplot=plot(0:0.01:0.99,p0list3,0:0.01:0.99,p1list3);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0:0.01:0.99,revlist3,0:0.01:0.99,surlist3,0:0.01:0.99,revlist3+surlist3);
xlabel('\alpha','FontSize',18)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)

%npv
npvalue0=(log(1+k1(1))/lambda1(1)-surplus0-x0(1)*demand0)/(1-demand0);
npvalue1=(log(1+k1(2))/lambda1(2)-surplus1-x0(2)*demand1)/(1-demand1);
delta0=abs(npvalue0-npvalue1);
p0list4=[];
p1list4=[];
revlist4=[];
surlist4=[];
for a=0:0.01:0.99
    tol=(1-a)*delta0;
    [price, rev]=logisticprice_npv(x0,d,k1,lambda1,cost,tol,[],[],[0 0],[100 100]);
    rev=-rev;
    sur=d(1)*log(1+k1(1)*exp(-lambda1(1)*price(1)))/lambda1(1)+d(2)*log(1+k1(2)*exp(-lambda1(2)*price(2)))/lambda1(2);
    p0list4=[p0list4 price(1)];
    p1list4=[p1list4 price(2)];
    revlist4=[revlist4 rev];
    surlist4=[surlist4 sur];
end
hplot=plot(0:0.01:0.99,p0list4,0:0.01:0.99,p1list4);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0:0.01:0.99,revlist4,0:0.01:0.99,surlist4,0:0.01:0.99,revlist4+surlist4);
xlabel('\alpha','FontSize',18)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)

plim=max([p1list p1list2 p1list3 p1list4 p0list p0list2 p0list3 p0list4]);
wellim=max([revlist+surlist revlist2+surlist2 revlist3+surlist3 revlist4+surlist4]);
figure
subplot(4,2,1);
hplot=plot(0:0.01:0.99,p0list,':b',0:0.01:0.99,p1list,'--r','LineWidth',2);
title('Price Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])

subplot(4,2,3);
hplot=plot(0:0.01:0.99,p0list2,':b',0:0.01:0.99,p1list2,'--r','LineWidth',2);
title('Demand Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])
subplot(4,2,5);
hplot=plot(0:0.01:0.99,p0list3,':b',0:0.01:0.99,p1list3,'--r','LineWidth',2);
title('Surplus Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])

subplot(4,2,7);
hplot=plot(0:0.01:0.99,p0list4,':b',0:0.01:0.99,p1list4,'--r','LineWidth',2);
title('No-purchase Valuation Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
ylim([0 plim+0.5])
legend(hplot,lh,'FontSize',16)

subplot(4,2,2);
hplot=plot(0:0.01:0.99,revlist,':m',0:0.01:0.99,surlist,'--g',0:0.01:0.99,revlist+surlist,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Price Fairness','FontSize',14)
%ylim([0 wellim+0.5])
subplot(4,2,4);
hplot=plot(0:0.01:0.99,revlist2,':m',0:0.01:0.99,surlist2,'--g',0:0.01:0.99,revlist2+surlist2,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Demand Fairness','FontSize',14)
%ylim([0 wellim+0.5])
subplot(4,2,6);
hplot=plot(0:0.01:0.99,revlist3,':m',0:0.01:0.99,surlist3,'--g',0:0.01:0.99,revlist3+surlist3,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Surplus Fairness','FontSize',14)
%ylim([0 wellim+0.5])
subplot(4,2,8);
hplot=plot(0:0.01:0.99,revlist4,':m',0:0.01:0.99,surlist4,'--g',0:0.01:0.99,revlist4+surlist4,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('No-purchase Valuation Fairness','FontSize',14)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
%ylim([0 wellim+0.5])
