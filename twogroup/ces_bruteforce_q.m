a=[1 2];
beta=[3 1.8];
d=[.1 .9];
cost=2;

accur=0.001;
q0=accur:accur:1;
q1=accur:accur:1;
q0=[10^(-8) 10^(-7) 10^(06) 10^(-5) q0];
q1=[10^(-8) 10^(-7) 10^(06) 10^(-5) q1];
rev=ones(length(q1));
demand=ones(length(q1));
sursum=ones(length(q1));
wel=ones(length(q1));
sumdiff=ones(length(q1));
npv=ones(length(q1));
pdiff=ones(length(q1));
mean0=beta(1)/(beta(1)-1)*a(1);
mean1=beta(2)/(beta(2)-1)*a(2);
for i=1:length(q1)
    for j=1:length(q1)
        p0=a(1)*q0(i)^(-1/beta(1));
        p1=a(2)*q1(j)^(-1/beta(2));
        pdiff(i,j)=abs(p0-p1);
        rev(i,j)=d(1)*(p0-cost)*q0(i)+d(2)*(p1-cost)*q1(j);
        demand(i,j)=abs(q0(i)-q1(j));
        sur0=beta(1)/(beta(1)-1)*a(1)*q0(i)^((beta(1)-1)/beta(1))-p0*q0(i);
        sur1=beta(2)/(beta(2)-1)*a(2)*q1(j)^((beta(2)-1)/beta(2))-p1*q1(j);
        sursum(i,j)=d(1)*sur0+d(2)*sur1;
        wel(i,j)=rev(i,j)+sursum(i,j);
        surdiff(i,j)=abs(sur0-sur1);
        npv0=(mean0-sur0-p0*q0(i))/(1-q0(i));
        npv1=(mean1-sur1-p1*q1(j))/(1-q1(j));
        npv(i,j)=abs(npv0-npv1);
    end
end

obj=max(rev(:));
[i,j]=find(rev==obj);

demand0=q0(i);
demand1=q1(j);
p0=a(1)*q0(i)^(-1/beta(1));
p1=a(2)*q1(j)^(-1/beta(2));
x0=[p0 p1];
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
    p0=a(1)*q0(i)^(-1/beta(1));
    p1=a(2)*q1(j)^(-1/beta(2));
    p0list=[p0list p0];
    p1list=[p1list p1];
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
demand0=min(1,(a(1)/x0(1))^beta(1));
demand1=min(1,(a(2)/x0(2))^beta(2));
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
    p0=a(1)*q0(i)^(-1/beta(1));
    p1=a(2)*q1(j)^(-1/beta(2));
    p0list2=[p0list2 p0];
    p1list2=[p1list2 p1];
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
surplus0=beta(1)/(beta(1)-1)*a(1)*demand0^((beta(1)-1)/beta(1))-x0(1)*demand0;
surplus1=beta(2)/(beta(2)-1)*a(2)*demand1^((beta(2)-1)/beta(2))-x0(2)*demand1;
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
    p0=a(1)*q0(i)^(-1/beta(1));
    p1=a(2)*q1(j)^(-1/beta(2));
    p0list3=[p0list3 p0];
    p1list3=[p1list3 p1];
    revlist3=[revlist3 rev(i,j)];
    surlist3=[surlist3 sursum(i,j)];
    wellist3=[wellist3 wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list3,0.01:0.01:1,p1list3);
xlabel('\alpha','FontSize',18)
ylim([0,10]);
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)



hplot=plot(0.01:0.01:1,revlist3,0.01:0.01:1,surlist3,0.01:0.01:1,wellist3);
xlabel('\alpha','FontSize',18)


%npv

npvalue0=(mean0-surplus0-x0(1)*demand0)/(1-demand0);
npvalue1=(mean1-surplus1-x0(2)*demand1)/(1-demand1);
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
    p0=a(1)*q0(i)^(-1/beta(1));
    p1=a(2)*q1(j)^(-1/beta(2));
    p0list4=[p0list4 p0];
    p1list4=[p1list4 p1];
    revlist4=[revlist4 rev(i,j)];
    surlist4=[surlist4 sursum(i,j)];
    wellist4=[wellist4 wel(i,j)];
end

hplot=plot(0.01:0.01:1,p0list4,0.01:0.01:1,p1list4);
xlabel('\alpha','FontSize',18)
ylim([0,10])
lh = {'p_0', 'p_1'};
legend(hplot,lh,'FontSize',16)

hplot=plot(0.01:0.01:1,revlist4,0.01:0.01:1,surlist4,0.01:0.01:1,wellist4);
xlabel('\alpha','FontSize',18)






plim=max(7);
wellim=max([revlist+surlist revlist2+surlist2 revlist3+surlist3 revlist4+surlist4])-0.4;
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
title('No-Purchase Valuation Fairness','FontSize',14)
xlabel('\alpha','FontSize',18)
lh = {'p_0', 'p_1'};
ylim([0 plim+0.5])
xlim([0 1])
legend(hplot,lh,'FontSize',16)

subplot(4,2,2);
hplot=plot(0:0.01:0.99,revlist,':m',0:0.01:0.99,surlist,'--g',0:0.01:0.99,revlist+surlist,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Price Fairness','FontSize',14)
ylim([0 wellim+0.5])
subplot(4,2,4);
hplot=plot(0:0.01:0.99,revlist2,':m',0:0.01:0.99,surlist2,'--g',0:0.01:0.99,revlist2+surlist2,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Demand Fairness','FontSize',14)
ylim([0 wellim+0.5])
subplot(4,2,6);
hplot=plot(0:0.01:0.99,revlist3,':m',0:0.01:0.99,surlist3,'--g',0:0.01:0.99,revlist3+surlist3,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('Surplus Fairness','FontSize',14)
ylim([0 wellim+0.5])

subplot(4,2,8);
hplot=plot(0:0.01:0.99,revlist4,':m',0:0.01:0.99,surlist4,'--g',0:0.01:0.99,revlist4+surlist4,'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('No-Purchase Valuation Fairness','FontSize',14)
lh = {'Profit', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
xlim([0 1])
ylim([0 wellim+0.5])

subplot(4,2,8);
hplot=plot(0:0.01:0.64,revlist4(1:65),':m',0:0.01:0.64,surlist4(1:65),'--g',0:0.01:0.64,revlist4(1:65)+surlist4(1:65),'-c','LineWidth',2);
xlabel('\alpha','FontSize',18)
title('No-purchase Valuation Fairness','FontSize',14)
lh = {'Profit', 'Surplus','Welfare'};
legend(hplot,lh,'FontSize',16)
xlim([0 1])
ylim([0 wellim+0.5])
