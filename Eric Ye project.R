setwd("C:\\Users\\ericy\\Documents\\METRO\\R for Data Analytics\\project")
getwd()

#read file
D = read.delim("xAPI-Edu-Data.csv", sep=",")
#view file
View(D)

#make sure it's data frame
is.data.frame(D)

#attach D
attach(D)

#get all variable names
names(D)

# dimensions and summary
dim(D)
summary(D)

# view settings
par()
#save original settings
opar=par()




#first add levels to class, L = low (0-69), M = medium (70-89), H = high (90-100)
D$Class = factor(D$Class,levels=c("L","M","H"))
D$Class



#1: Relation between raised hands and grade level
#Boxplot
boxplot(raisedhands~Class,data=D, main="Questions and Grades",xaxt = "n",
        xlab="Grade Level", ylab="Num. of Raised Hands",col=c("skyblue","blue","darkblue"))
axis(1,at=1:3,labels=c("Low","Mid","High"))



#2: Relation between visited resources and grade level
#boxplot
boxplot(VisITedResources~Class,data=D, main="Resources and Grades",xaxt = "n",
        xlab="Grade Level", ylab="Visited Resources",col=c("pink","salmon","red"))
axis(1,at=1:3,labels=c("Low","Mid","High"))



#3: Relation between Announcements viewed and grade level
#barplot
boxplot(AnnouncementsView~Class,data=D, main="Announcements and Grades",xaxt = "n",
        xlab="Grade Level", ylab="Viewed Announcements",col=c("yellow","gold","orange"))
axis(1,at=1:3,labels=c("Low","Mid","High"))



#4: Relation between Discussion and Grade level
#boxplot
boxplot(Discussion~Class,data=D, main="Discussion and Grades",xaxt = "n",
        xlab="Grade Level", ylab="Discussions participated in",col=c("mediumspringgreen","green","limegreen"))
axis(1,at=1:3,labels=c("Low","Mid","High"))



#5: Participation vs grade  
#Violin plot
install.packages("vioplot")
library(vioplot)
#create participation variable
D$participation = D$raisedhands + D$VisITedResources + D$AnnouncementsView + D$Discussion

#take participation levels from grade groups
x1 = D$participation[D$Class=="L"]
x2 = D$participation[D$Class=="M"]
x3 = D$participation[D$Class=="H"]
vioplot(x1, x2, x3, ylab="Num. of Participations", xlab="Grade Level",names=c("Low", "Medium", "High"), col="gold")
title("Class Participation and Grades")



#6: Plot of absences by Grade Level
#Barplot
counts = table(D$StudentAbsenceDays, D$Class)
counts
barplot(counts, main="Grade Distribution by Absences",xaxt = "n",
        xlab="Grade Level",ylab="Absences", col=c("darkblue","gold"))
axis(1,at=1:3,labels=c("Low","Mid","High"))
legend("topright", fill=c("darkblue","gold"), rownames(counts),cex=0.75)



#7:Participation vs grades for each school level
#Violin plots
#set up elementary school
l1 = D$participation[D$Class=="L" & D$StageID=="lowerlevel"]
l2 = D$participation[D$Class=="M" & D$StageID=="lowerlevel"]
l3 = D$participation[D$Class=="H" & D$StageID=="lowerlevel"]

#set up middle school
m1 = D$participation[D$Class=="L" & D$StageID=="MiddleSchool"]
m2 = D$participation[D$Class=="M" & D$StageID=="MiddleSchool"]
m3 = D$participation[D$Class=="H" & D$StageID=="MiddleSchool"]

#set up middle school
h1 = D$participation[D$Class=="L" & D$StageID=="HighSchool"]
h2 = D$participation[D$Class=="M" & D$StageID=="HighSchool"]
h3 = D$participation[D$Class=="H" & D$StageID=="HighSchool"]

#3 graphs on same page
par(mfrow=c(3,1))

vioplot(l1, l2, l3, ylab="Num. of Participations", xlab="Grade Level",names=c("Low", "Medium", "High"), col="green")
title("Class Participation and Grades in Elementary school")

vioplot(m1, m2, m3, ylab="Num. of Participations", xlab="Grade Level",names=c("Low", "Medium", "High"), col="purple")
title("Class Participation and Grades in Middle school")

vioplot(h1, h2, h3, ylab="Num. of Participations", xlab="Grade Level",names=c("Low", "Medium", "High"), col="gold")
title("Class Participation and Grades in High school")

#return to original settings
par()=par(opar)



#8:Parents satisfaction affecting grade levels
#barplot
count2 = table(D$ParentschoolSatisfaction, D$Class)
count2
barplot(count2, main="Grade Distribution by Parent Ratings",xaxt = "n",
        xlab="Grade Level",ylab="Parent Rating", col=c("red","green"))
legend("topright", fill=c("red","green"), rownames(count2),cex=0.75)
axis(1,at=1:3,labels=c("Low","Mid","High"))



#9: Grade Levels for Gender
#Barplot
count3 = table(D$gender, D$Class)
count3
barplot(count3, main="Grade Distribution by Gender",xaxt = "n",
        xlab="Grade Level",ylab="Frequency", col=c("pink","blue"))
axis(1,at=1:3,labels=c("Low","Mid","High"))
legend("topright", fill=c("pink","blue"), rownames(count3),cex=0.75)



#10: Grade Levels for Primary Caregiver and Gender- Proportional
#Plot
count4 = table(D$gender, D$Relation, D$Class)
count4
plot(count4,col=c("salmon","yellow","lightgreen"),main="Primary caregiver, gender, and grades",xlab = "Gender",ylab = "Parent")


#11: First vs Second Term
#Notched Box Plot
boxplot(D$participation~D$Semester*D$Class, data=D, notch=TRUE,xaxt = "n",
        col=(c("skyblue","red")),
        main="First vs Second Term", xlab="Grade Level per Semester",ylab="Participation scores")
axis(1,at=c(1.5,3.5,5.5),labels=c("Low","Mid","High"))
legend("topleft", fill=c("skyblue","red"), legend= (c("1st Term","2nd Term")),cex=0.6)

