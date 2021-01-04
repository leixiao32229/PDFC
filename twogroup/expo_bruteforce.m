
lam0=1;lam1=0.2;
d0=0.5;d1=0.5;
c=0.5;
x0=1/lam0+c; 
x1=1/lam1+c;
accur=0.01;
p0=accur:accur:(2*x1);
p1=accur:accur:(2*x1);
rev=ones(length(p1));
demand=ones(length(p1));
sursum=ones(length(p1));
wel=ones(length(p1));
sumdiff=ones(length(p1));
npv=ones(length(p1));
for i=1:length(p1)
    for j=1:length(p1)
        rev(i,j)=d0*(p0(i)-c)*exp(-lam0*p0(i))+d1*(p1(j)-c)*exp(-lam1*p1(j));
        demand(i,j)=abs(exp(-lam0*p0(i))-exp(-lam1*p1(j)));
        sursum(i,j)=d0*exp(-lam0*p0(i))/lam0+d1*exp(-lam1*p1(j))/lam1;
        wel(i,j)=rev(i,j)+sursum(i,j);
        surdiff(i,j)=abs(d0*exp(-lam0*p0(i))/lam0-d1*exp(-lam1*p1(j))/lam1);
        npv0=1/lam0-(p0(i)*exp(-lam0*p0(i)))/(1-exp(-lam0*p0(i)));
        npv1=1/lam1-(p1(j)*exp(-lam1*p1(j)))/(1-exp(-lam1*p1(j)));
        npv(i,j)=abs(npv0-npv1);
    end
end



%demand
delta0=demand(x0/accur,x1/accur);
p0list2=[];
p1list2=[];
revlist2=[];
surlist2=[];
wellist2=[];
for a=0:0.01:0.99
    tol=delta0*(1-a);
    judge=demand<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
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
delta0=surdiff(x0/accur,x1/accur);
p0list3=[];
p1list3=[];
revlist3=[];
surlist3=[];
wellist3=[];
for a=0:0.01:0.99
    tol=delta0*(1-a);
    judge=surdiff<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
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
delta0=surdiff(x0/accur,x1/accur);
p0list4=[];
p1list4=[];
revlist4=[];
surlist4=[];
wellist4=[];
for a=0:0.01:0.99
    tol=delta0*(1-a);
    judge=npv<=tol;
    aaa=rev.*judge;
    obj=max(aaa(:));
    [i,j]=find(aaa==obj);
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


