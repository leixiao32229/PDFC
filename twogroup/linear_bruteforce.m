
b=[2 4.5];
d=[0.35 0.65];
cost=0.6;

accur=0.001;
p0=accur:accur:b(2);
p1=accur:accur:b(2);
rev=ones(length(p1));
demand=ones(length(p1));
sursum=ones(length(p1));
wel=ones(length(p1));
sumdiff=ones(length(p1));
npv=ones(length(p1));
pdiff=ones(length(p1));
mean0=b(1)/2;
mean1=b(2)/2;
for i=1:length(p1)
    for j=1:length(p1)
        pdiff(i,j)=abs(p0(i)-p1(j));
        q0=max(1-p0(i)/b(1),0);
        q1=max(1-p1(j)/b(2),0);
        rev(i,j)=d(1)*(p0(i)-cost)*q0+d(2)*(p1(j)-cost)*q1;
        demand(i,j)=abs(q0-q1);
        sur0=(b(1)-p0(i))*q0/2;
        sur1=(b(2)-p1(j))*q1/2;
        sursum(i,j)=d(1)*sur0+d(2)*sur1;
        wel(i,j)=rev(i,j)+sursum(i,j);
        surdiff(i,j)=abs(sur0-sur1);
        npv0=(mean0-sur0-p0(i)*q0)/(1-q0);
        npv1=(mean1-sur1-p1(j)*q1)/(1-q1);
        npv(i,j)=abs(npv0-npv1);
    end
end

obj=max(rev(:));
[i,j]=find(rev==obj);
x0=[p0(i) p1(j)];
%price
delta0=abs(x0(1)-x0(2));
p0list=[];
p1list=[];
revlist=[];
surlist=[];
wellist=[];
for a0=0:0.01:0.99
    tol=delta0*(1-a0);
    judge=pdiff<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
    i=i(1);
    j=j(1);
    p0list=[p0list p0(i)];
    p1list=[p1list p1(j)];
    revlist=[revlist rev(i,j)];
    surlist=[surlist sursum(i,j)];
    wellist=[wellist wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list,0.01:0.01:1,p1list);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0.01:0.01:1,revlist,0.01:0.01:1,surlist,0.01:0.01:1,wellist);
xlabel('\alpha','FontSize',18)


%demand
demand0=max(1-x0(1)/b(1),0);
demand1=max(1-x0(2)/b(2),0);
delta0=abs(demand0-demand1);
p0list2=[];
p1list2=[];
revlist2=[];
surlist2=[];
wellist2=[];
for a0=0:0.01:0.99
    tol=delta0*(1-a0);
    judge=demand<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
    i=i(1);
    j=j(1);
    p0list2=[p0list2 p0(i)];
    p1list2=[p1list2 p1(j)];
    revlist2=[revlist2 rev(i,j)];
    surlist2=[surlist2 sursum(i,j)];
    wellist2=[wellist2 wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list2,0.01:0.01:1,p1list2);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)



hplot=plot(0.01:0.01:1,revlist2,0.01:0.01:1,surlist2,0.01:0.01:1,wellist2);
xlabel('\alpha','FontSize',18)


%Surplus
surplus0=(b(1)-cost)^2/8/b(1);
surplus1=(b(2)-cost)^2/8/b(2);
delta0=abs(surplus0-surplus1);
p0list3=[];
p1list3=[];
revlist3=[];
surlist3=[];
wellist3=[];
for a0=0:0.01:0.99
    tol=delta0*(1-a0);
    judge=surdiff<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
    i=i(1);
    j=j(1);
    p0list3=[p0list3 p0(i)];
    p1list3=[p1list3 p1(j)];
    revlist3=[revlist3 rev(i,j)];
    surlist3=[surlist3 sursum(i,j)];
    wellist3=[wellist3 wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list3,0.01:0.01:1,p1list3);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)



hplot=plot(0.01:0.01:1,revlist3,0.01:0.01:1,surlist3,0.01:0.01:1,wellist3);
xlabel('\alpha','FontSize',18)


%npv

npvalue0=x0(1)/2;
npvalue1=x0(2)/2;
delta0=abs(npvalue0-npvalue1);
p0list4=[];
p1list4=[];
revlist4=[];
surlist4=[];
wellist4=[];
for a0=0:0.01:0.99
    tol=delta0*(1-a0);
    judge=npv<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
    i=i(1);
    j=j(1);
    p0list4=[p0list4 p0(i)];
    p1list4=[p1list4 p1(j)];
    revlist4=[revlist4 rev(i,j)];
    surlist4=[surlist4 sursum(i,j)];
    wellist4=[wellist4 wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list4,0.01:0.01:1,p1list4);
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)



hplot=plot(0.01:0.01:1,revlist4,0.01:0.01:1,surlist4,0.01:0.01:1,wellist4);
xlabel('\alpha','FontSize',18)

for i=44:100
    p0list(i)=2.35;
    p1list(i)=2.35;
end


plim=max([p1list p1list2 p1list3 p1list4 p0list p0list2 p0list3 p0list4]);
wellim=max([revlist+surlist revlist2+surlist2 revlist3+surlist3 revlist4+surlist4]);
wellim=0.7
figure
subplot(4,2,1);
plot(0.01:0.01:0.22,p0list(1:22),':b',0.22:0.01:1,2.55*ones(79)-0.02,':b',0.01:0.01:0.22,p1list(1:22),'--r',0.22:0.01:1,2.55*ones(79)-0.02+0.02,'--r','LineWidth',2)
hold on
plot(0.215,2.55,'ob',0.215,2.55,'or')
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
hplot=plot(0:0.01:0.99,revlist,':m',0:0.01:0.99,surlist,'--g',0:0.01:0.99,wellist,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Price Fairness','FontSize',14)
ylim([0 wellim+0.5])
subplot(4,2,4);
hplot=plot(0:0.01:0.99,revlist2,':m',0:0.01:0.99,surlist2,'--g',0:0.01:0.99,wellist2,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Demand Fairness','FontSize',14)
ylim([0 0.9])
subplot(4,2,6);
hplot=plot(0:0.01:0.99,revlist3,':m',0:0.01:0.99,surlist3,'--g',0:0.01:0.99,wellist3,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Surplus Fairness','FontSize',14)
ylim([0 wellim+0.5])
subplot(4,2,8);
hplot=plot(0:0.01:0.99,revlist4,':m',0:0.01:0.99,surlist4,'--g',0:0.01:0.99,wellist4,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('No-purchase Valuation Fairness','FontSize',14)
lh = {'Profit', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
ylim([0 wellim+0.5])





figure
subplot(2,2,1);
plot(0.01:0.01:0.43,p0list(1:43),':b',0.43:0.01:1,[2.35 p0list(44:100)]-0.02,':b',0.01:0.01:0.43,p1list(1:43),'--r',0.43:0.01:1,[2.35 p1list(44:100)-0.01]+0.02,'--r','LineWidth',2)
hold on
plot(0.425,2.35,'ob',0.425,2.35,'or')
title('Price Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])

subplot(2,2,2);
hplot=plot(0:0.01:0.99,p0list2,':b',0:0.01:0.99,p1list2,'--r','LineWidth',2);
title('Demand Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])
subplot(2,2,3);
hplot=plot(0:0.01:0.99,p0list3,':b',0:0.01:0.99,p1list3,'--r','LineWidth',2);
title('Surplus Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
ylim([0 plim+0.5])

subplot(2,2,4);
hplot=plot(0:0.01:0.99,p0list4,':b',0:0.01:0.99,p1list4,'--r','LineWidth',2);
title('No-purchase Valuation Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
ylim([0 plim+0.5])
legend(hplot,lh,'FontSize',16)



figure
subplot(2,2,1);
hplot=plot(0:0.01:0.99,revlist,':m',0:0.01:0.99,surlist,'--g',0:0.01:0.99,wellist,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Price Fairness','FontSize',14)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
%ylim([0 wellim+0.5])
subplot(2,2,2);
hplot=plot(0:0.01:0.99,revlist2,':m',0:0.01:0.99,surlist2,'--g',0:0.01:0.99,wellist2,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Demand Fairness','FontSize',14)
%ylim([0 wellim+0.5])
subplot(2,2,3);
hplot=plot(0:0.01:0.99,revlist3,':m',0:0.01:0.99,surlist3,'--g',0:0.01:0.99,wellist3,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Surplus Fairness','FontSize',14)
%ylim([0 wellim+0.5])
subplot(2,2,4);
hplot=plot(0:0.01:0.99,revlist4,':m',0:0.01:0.99,surlist4,'--g',0:0.01:0.99,wellist4,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('No-purchase Valuation Fairness','FontSize',14)
lh = {'Revenue', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
%ylim([0 wellim+0.5])