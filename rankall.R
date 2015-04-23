rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        ## Read outcome data
        outcome.dat<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        nr<-nrow(outcome.dat)
        nc<-ncol(outcome.dat)
        u.state<-unique(outcome.dat[,7])
        u.state<-sort(u.state)
        n.state=length(u.state)
        u.outcome<-c("heart attack","heart failure","pneumonia")
        n.outcome=length(u.outcome)
        
        #Check outcome is valid
        oc.test<-logical()
        for(i in 1:n.outcome){
                oc.test[i]=identical(outcome,u.outcome[i])
        }
        if(sum(oc.test)==0) {stop("invalid outcome")}
        
        #set the column number for the required outcome
        if(outcome=="heart attack"){
                col<-11
        }
        else if (outcome=="heart failure"){
                col<-17
        }
        else if (outcome=="pneumonia"){
                col<-23    
        }
  
        ## For each state, find the hospital of the given rank
        
        hosp<-character()
        for (i in 1:n.state){
                #create a data frame with 3 columns (State, Hospital, The required outcome)
                #first remove rows with missing values
                ss<-outcome.dat[outcome.dat[,7]==u.state[i] & outcome.dat[,col]!="Not Available",]
                ss.df<-data.frame(ss[,7],as.character(ss[,2]),as.numeric(ss[,col]))
                colnames(ss.df)<-c("State","Hospital","Mortality_Rate")
                sorted.ss<-ss.df[with(ss.df,order(Mortality_Rate,Hospital)),]
                # Assign ranks to "worst" and "best"
                
                nr<-nrow(sorted.ss)
                rank<-num
                if(num=="best") {
                        rank<-1
                }
                if(num=="worst") {
                        rank<-nr
                }
                
 
                hosp[i]<-as.character(sorted.ss[rank,2])

        }        

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        rankall.op<-data.frame(hosp,u.state)
        colnames(rankall.op)<-c("hospital","state")
        rankall.op

}