
# ----------------------------------------------------------------------------
# Description: R Commander Plug-in for repeated-measures ANOVA
# ----------------------------------------------------------------------------
# Summary: Adds a new menu entry for repeated measures that allows 
#          to deal with up to three within-subject factors and optionnally with 
#          one or several between-subject factors.
#          It also provides supplmementary options to oneWayAnova() and 
#          multiWayAnova() functions, such as choice of anova type, display of 
#          effect sizes and post hoc analysis for multiWayAnova.
# ----------------------------------------------------------------------------

# The following is added to remove a NOTE in cran check
utils::globalVariables(c(".myAnova", "buttonsFrame", "checkBoxFrame","dataTab", 
                         "effectSizeVariable", "formulaFrame", 
                         "groupSummaryVariable", "lhsEntry", "lhsVariable", 
                         "mulCompVariable",
                         "notebook", "onHelp", "outerOperatorsFrame", 
                         "optionsTab", "rhsVariable", "SStypeButtonsFrame", 
                         "SStypeButtonsVariable", "subsetFrame", 
                         "subsetVariable",  "top", "xBox"))


# This function causes the package to load Rcmdr if it is not already loaded.
# It has been copied from RcmdrPlugin.Teachingdemos package

.onAttach <- function(libname, pkgname){
  if (!interactive()){
    return()
  }
  putRcmdr("slider.env", new.env())
  Rcmdr   <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr = Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    } else {
        Commander()
      }
  }
}

#' Repeated measure ANOVA setup
#'
#' Dialog box to enter the names and levels of within-factors.
#' 
#' @details 
#' Up to three factors can be entered. A valid within-factor entry must 
#' consist in a syntactically valid name (see \link{make.names}) and 2 
#' levels or more.
#' 
#' On OK:
#' \itemize{
#'  \item {} {The first valid entries are kept and stored in 
#'  \code{.withinfactors} and \code{.withinlevels} for factor names and 
#'  levels, respectively.} 
#'  \item {} {The next dialog box (\code{repMeasAnova(.withinfactors, .withinfactors)}
#'   is launched.
#'  }
#' }
#'
#' @seealso \link{repMeasAnova}.
#' 
#' @author Jessica Mange \email{jessica.mange@@unicaen.fr}
#' @author Arnaud Travert \email{arnaud.travert@@unicaen.fr}

repMeasAnovaSetup <- function () {
  
  defaults  <- list(initial.withinfactors 
                    = c(gettext("factor_1", 
                                domain="R-RcmdrPlugin.aRnova"), "", ""), 
                        initial.betweenlevels = c("2", "", ""))
  
  dialog.values <- getDialog("Definition of Within-subject factors.", defaults)
  
  initializeDialog(title=gettext("Definition of Within-subject factors.", 
                                 domain="R-RcmdrPlugin.aRnova"))
  
  nameW1        <- tclVar(dialog.values$initial.withinfactors[1])
  entrynameW1   <- ttkentry(top, width="20", textvariable=nameW1)
  nameW2        <- tclVar(dialog.values$initial.withinfactors[2])
  entrynameW2   <- ttkentry(top, width="20", textvariable=nameW2)
  nameW3        <- tclVar(dialog.values$initial.withinfactors[3])
  entrynameW3   <- ttkentry(top, width="20", textvariable=nameW3)
  
  levelsW1      <- tclVar(dialog.values$initial.betweenlevels[1])
  entrylevelsW1 <- ttkentry(top, width="4", textvariable=levelsW1)
  levelsW2      <- tclVar(dialog.values$initial.betweenlevels[2])
  entrylevelsW2 <- ttkentry(top, width="4", textvariable=levelsW2)
  levelsW3      <- tclVar(dialog.values$initial.betweenlevels[3])
  entrylevelsW3 <- ttkentry(top, width="4", textvariable=levelsW3)

  #' @import tcltk Rcmdr
  #' 
   
  onOK <- function()
    {
    # factor names and makes a list
    nameW1Value    <- trim.blanks(tcltk::tclvalue(nameW1))
    nameW2Value    <- trim.blanks(tcltk::tclvalue(nameW2))
    nameW3Value    <- trim.blanks(tcltk::tclvalue(nameW3))
    .withinfactors <- c(nameW1Value, nameW2Value, nameW3Value)

    # number of levels for each within factors
    levelsW1Value <- as.numeric(tcltk::tclvalue(levelsW1))
    levelsW2Value <- as.numeric(tcltk::tclvalue(levelsW2))
    levelsW3Value <- as.numeric(tcltk::tclvalue(levelsW3))
    .withinlevels <- c(levelsW1Value, levelsW2Value, levelsW3Value)

    # check that the first entry is correct
    if ((.withinfactors[1]  == "") | (.withinlevels[1] < 2) 
        | is.na(.withinlevels[1])) {
      errorCondition(recall  = repMeasAnovaSetup,
                     message =
                       gettext(
                         "You must define at least one within subject variable.", 
                                       domain="R-RcmdrPlugin.aRnova"))
      return()
     }

    if (!is.valid.name(.withinfactors[1])){
      errorCondition(recall  = repMeasAnovaSetup,
                     message = paste('"', .withinfactors[1], '" ',
                                     gettextRcmdr("is not a valid name."), sep=""))
      return()
    }

    validentry <- T
    i <- 1
    while (validentry) {
      i <- i + 1
      if ((i==4) | (.withinfactors[i]  == "") |  (.withinlevels[i]  < 2)
           | (!is.valid.name(.withinfactors[i]) | is.na(.withinlevels[i]))) {
          validentry <- F
      }
    }

    .withinfactors  <- .withinfactors[1:i-1]
    .withinlevels <- .withinlevels[1:i-1]
    
    #prevents getting "NaN" on empty entries upon reset 
    if (is.na(levelsW2Value)) levelsW2Value = ""
    if (is.na(levelsW3Value)) levelsW3Value = ""
    
    putDialog("Definition of Within-subject factors.", 
              list(initial.withinfactors = c(nameW1Value, nameW2Value, nameW3Value), 
                   initial.betweenlevels = c(toString(levelsW1Value), 
                                             toString(levelsW2Value), 
                                             toString(levelsW3Value))))
    closeDialog()
    
    # reset repMeasAnovadialog
    defaults      <- list(initial.withinSelection = NULL, 
                          initial.betweenSelection = NULL, 
                          initial.SStype = "2", 
                          initial.checkBox = c("0", "0", "0"),
                          initial.tab = 0)
    putDialog("repMeasAnova", defaults)
    
    repMeasAnova(.withinfactors, .withinlevels)
  }
  
  tcltk::tkfocus(CommanderWindow())

  OKCancelHelp(helpSubject="repMeasAnovaSetup", reset=".repMeasAnovaSetupReset")

  leftlabel  <- labelRcmdr(top, text = gettext("Factor Name", 
                                               domain="R-RcmdrPlugin.aRnova"),
                           fg = getRcmdr("title.color"), font="RcmdrTitleFont")
  rightlabel <- labelRcmdr(top, text = gettext("Number of levels", 
                                               domain="R-RcmdrPlugin.aRnova"),
                           fg = getRcmdr("title.color"), font="RcmdrTitleFont")
  tkgrid(leftlabel, rightlabel)
  tkgrid.configure(leftlabel, sticky="w")
  tkgrid.configure(rightlabel, sticky="w")

  tkgrid(entrynameW1, entrylevelsW1)
  tkgrid.configure(entrynameW1, sticky="w")
  tkgrid.configure(entrylevelsW1, sticky="w")

  tkgrid(entrynameW2, entrylevelsW2)
  tkgrid.configure(entrynameW2, sticky="w")
  tkgrid.configure(entrylevelsW2, sticky="w")

  tkgrid(entrynameW3, entrylevelsW3)
  tkgrid.configure(entrynameW3, sticky="w")
  tkgrid.configure(entrylevelsW3, sticky="w")

  tkgrid(buttonsFrame, columnspan="4", sticky="w")
  dialogSuffix()
}

.repMeasAnovaSetupReset <- function () {

  defaults  <- list(initial.withinfactors = c(gettext("factor_1", domain="R-RcmdrPlugin.aRnova"), "", ""), 
                    initial.betweenlevels = c("2", "", ""))
  
  putDialog("Definition of Within-subject factors.", defaults)
        
  repMeasAnovaSetup()
}

#' Repeated measures ANOVA
#'
#' Dialog box to (i) select the within-subject variables corresponding
#' to the factors defined in  \code{\link{repMeasAnovaSetup}}, (ii) select the
#' between-suject factors, (iii) set options and (iv) launch the repeated 
#' measures anova.
#' 
#' @param .withinfactors list of within-subject factors
#' @param .withinlevels list of within-subject variables
#' 
#' @details
#' Options:
#' \itemize{
#' \item{\code{'SS type'}} {type of sum of squares, default: \code{type = 2}. 
#' See Details in \code{\link[car]{Anova}}}
#' \item{\code{'Effect size'}} {compute and prints effect size (partial eta squared)}
#' \item{\code{'Summary statistics for groups'}} {prints summary statistics for
#' groups formed by all combinations of factors}
#' \item{\code{'Pairwise comparisons of means'}} {performs post-hoc Tukey's HSD test  
#' on significant (p < .05) or close to significant (p < 0.1) effects.}  
#' }
#' On OK, the following operations are carried out:
#' \itemize{
#'  \item {} {Generates a dataset containing complete cases and converted 
#'  from 'wide' to 'long' format (extension  \code{.cplt.lg}), with the following columns added:
#'    \itemize{
#'      \item{\code{'id'} (factor)} {identifies the subjects.}
#'      \item{\code{'DV'} (numeric)} {the measure or dependent variable.}
#'      \item{\code{'trial'} (int)} {variable that differentiates multiple 
#'      measures (\code{'DV'}) from the same subject (\code{'id'}).}
#'      \item {\code{'<factorA>'} (factor)} {levels of the 
#'      within-suject factor A (one column per within subject factor)}
#'      \item {\code{'<factorA.factorB:...>'} (factor)} {factor that 
#'      differentiates multiple measures from groups or subjects with same factors 
#'      levels}
#'    }
#'  This 'long' dataset is useful for ploting means and post-hoc analysis  
#'  }
#'  \item{} {Computes repeated measure ANOVA using \code{\link[car]{Anova}}} 
#'  \item{} {Computes effect sizes (partial eta squared)}
#'  \item{} {Prints a summary of marginal statistics 
#'  (count, min, max, mean, ds)}
#'  \item{} {runs post-hoc analysis on significant or close to significant effects}
#' }
#' 
#' @return None
#'
#' @seealso \code{\link{repMeasAnovaSetup}} for the definition of 
#' within factors, \code{\link[car]{Anova}} for the computation of ANOVA
#' 
#' @author Jessica Mange \email{jessica.mange@@unicaen.fr}
#' @author Arnaud Travert \email{arnaud.travert@@unicaen.fr}

repMeasAnova <- function(.withinfactors, .withinlevels){
  
  # outputformats of doItAndPrint() and functions alike for the list of 
  # within-subject factors:
  #
  #   withinfactors_list : factorA, factorB, factorC
  #   withinfactors_listr: "factorA", "factorB", "factorC" 
  #  
  # (note: the same terminations (_list, _listr) will be used to 
  # denote the same formats of other lists)
  #
  
  withinfactors_list  <- paste(paste(.withinfactors, sep = ""), collapse = ", ")
  withinfactors_listr <- paste(paste("\"", .withinfactors, "\"", sep=""), 
                               collapse = ", ")

  defaults  <- list(initial.withinSelection = NULL, 
                    initial.betweenSelection = NULL, 
                    initial.SStype = "2", 
                    initial.checkBox = c("0", "0", "0"),
                    initial.tab = 0)
  
  dialog.values <- getDialog("repMeasAnova", defaults)
  
  tkadd <- tcltk::tkadd
  ttknotebook <- tcltk::ttknotebook
  
  initializeDialog(title = gettext("Repeated measures Analysis of Variance", 
                                   domain="R-RcmdrPlugin.aRnova"), use.tabs=TRUE)
  # Data Tab
  # ask for model name
  UpdateModelNumber()
  modelName  <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), sep = ""))
  modelFrame <- tkframe(dataTab)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)

  # ask select within-subject variables
  askwithin <- tklabel(dataTab, 
                       textvariable = 
                         tclVar(gettext("2. Select within-subject variables", 
                                        domain="R-RcmdrPlugin.aRnova")),
                       foreground="blue")

  # recall list of within-subject factors
  witinfactornamesFrame <- tkframe(dataTab)

  WithinFactorsNames <- tklabel(witinfactornamesFrame,
                                textvariable = 
                                  tclVar(paste(gettext("Factor(s):",
                                                       domain="R-RcmdrPlugin.aRnova"),
                                               withinfactors_list, sep = " ")))

  # Select within-subject variables
  dataFrame <- tkframe(dataTab)
  
  nfactor     <- length(.withinfactors)
  ncomblevels <- prod(.withinlevels)

  # initializes a list of combined levels
  tupleoflevels <- list()
  tupleoflevelnames <- list()
  # loops over all combinations to make the list
  #  t: tuple of levels
  # ii: index of tupleoflevels (1:ncomblevels)

  t <- 0
  tname <- ""
  ii <- 1

  for (i in 1:.withinlevels[1]) {
    t <- i
    tname <- paste(.withinfactors[1], "_", i, sep="")
    if (nfactor > 1) {
      t <- c(t, 0)
      tname <- c(tname, "")
      for (j in 1:.withinlevels[2]) {
        t[2] <- j
        tname[2] <- paste(.withinfactors[2], "_", j, sep="")
        if (nfactor > 2) {
          for (k in 1:.withinlevels[3]) {
            t[3] <- k
            tname[3] <- paste(.withinfactors[3], "_", k, sep="")
            tupleoflevels[[ii]] <- t
            tupleoflevelnames[[ii]] <- tname
            ii <- ii + 1
          }
        } else {
          tupleoflevels[[ii]] <- t
          tupleoflevelnames[[ii]] <- tname
          ii <- ii + 1
          }
      }
    } else {
      tupleoflevels[[ii]] <- t
      tupleoflevelnames[[ii]] <- tname
      ii <- ii + 1
    }
  }
  
  # concatenates level names as:  "factor_A_1 & factor_B_2", ....
  levelnamesANDstring <- list()
  for (i in 1:length(tupleoflevelnames)) {
    levelnamesANDstring[i] <- paste(paste(tupleoflevelnames[[i]], 
                                          sep = ""), collapse = " & ")
  }

  comboboxwithin <- list()
  texts         <- list()
  withinSelection   <- NULL
  for (i in 1:ncomblevels) {
    text <- paste(gettext(" ", domain="R-RcmdrPlugin.aRnova"), 
                  levelnamesANDstring[i], sep=" ")
    labelText <- tclVar(text)
    texts[[i]] <- tklabel(dataFrame, textvariable = labelText)
    
    values <- gettext("--Choose one--", domain="R-RcmdrPlugin.aRnova")
    for (col_name in Numeric()) {
      values <- c(values, col_name)
    }
    
    withinSelection[[i]] <- tclVar()
    tclvalue(withinSelection[[i]]) <- values[1]
    if (length(dialog.values$initial.withinSelection[[i]]) > 0) { 
        withinSelection[[i]] <- dialog.values$initial.withinSelection[[i]]
    }
       
    comboboxwithin[[i]] <- tcltk::ttkcombobox(dataFrame, 
                                              textvariable = withinSelection[[i]], 
                                              values = values, state="readonly")
}
  # ask to select between-subject factors
  askbetweenFrame <- tkframe(dataTab)
  askbetween <- tklabel(askbetweenFrame, 
                        textvariable = 
                          tclVar(gettext("3. Select between-subject variables (pick none, one or more)",
                                         domain="R-RcmdrPlugin.aRnova")),
                        foreground="blue")

  #  list box Between subject factors
  dataFrame2 <- tkframe(dataTab)
  BetweenFactors <- tklabel(dataFrame2, 
                            textvariable = 
                              tclVar(gettext("Factor(s):", domain="R-RcmdrPlugin.aRnova")))

  listbox_between <- tklistbox(dataFrame2, height=5, selectmode="multiple", 
                               background="white", exportselection=0)
  for (col_name in Factors()) {
    tkinsert(listbox_between, "end", col_name)
  }
  for (num in as.numeric(dialog.values$initial.betweenSelection)) tkselection.set(listbox_between, num)
  
  # Options Tab
  opFrame <- tkframe(optionsTab)
  askSStype <- tklabel(opFrame, 
                       textvariable = 
                         tclVar(gettext("Sum of Squares type", 
                                        domain="R-RcmdrPlugin.aRnova")))
  
  radioButtons(window = opFrame, name="SStypeButtons", buttons=c("b2", "b3"), values=c("2", "3"), 
               initialValue = dialog.values$initial.SStype,
               labels=gettext(c("Type 2", "Type 3"), domain="R-RcmdrPlugin.aRnova"))
  
  checkBoxes(window = opFrame, frame="checkBoxFrame", boxes=c("effectSize", "groupSummary", "mulComp"), 
             initialValues = dialog.values$initial.checkBox, 
             labels=gettext(c("Effect size", "Summary statistics for groups", "Pairwise comparisons of means"), domain="R-RcmdrPlugin.aRnova"))


  onOK <- function() {
    
  tkselect <- tcltk::tkselect
  tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
  modelValue <- trim.blanks(tcltk::tclvalue(modelName))
  if (!is.valid.name(modelValue)) {
    UpdateModelNumber(-1)
    errorCondition(recall  = repMeasAnova(.withinfactors, .withinlevels),
                   message = 
                     sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                             modelValue))
    return()
  }
  if (is.element(modelValue, listAOVModels())) {
    if ("no" == tcltk::tclvalue(checkReplace(modelValue, 
                                      type = gettextRcmdr("Model")))) {
      UpdateModelNumber(-1)
      tkdestroy(top)
      repMeasAnova(.withinfactors, .withinlevels)
      return()
    }
  }
    repmeasures <- NULL
    for (i in 1:ncomblevels) {
      repmeasures <- c(repmeasures, tclvalue(withinSelection[[i]]))
    }
    repmeasures_list  <- paste(paste(repmeasures, sep = ""), collapse = ", ")
    repmeasures_listr <- paste(paste("\"", repmeasures, "\"", sep=""), 
                               collapse = ", ")

    betweenfactors <- Factors()[as.numeric(tkcurselection(listbox_between)) + 1]
    betweenfactors_list  <- paste(paste(betweenfactors, sep = ""), 
                                  collapse = ", ")

    SStypeVar       <- tclvalue(SStypeButtonsVariable)     
    effectSizeVar   <- tclvalue(effectSizeVariable)
    groupSummaryVar <- tclvalue(groupSummaryVariable)
    mulCompVar      <- tclvalue(mulCompVariable)

    putDialog ("repMeasAnova", 
               list(initial.withinSelection = withinSelection, 
                    initial.betweenSelection = tkcurselection(listbox_between), 
                    initial.SStype = SStypeVar, 
                    initial.checkBox = c(effectSizeVar, groupSummaryVar, mulCompVar),
                    initial.tab = tab))
    closeDialog()

    for (i in 1:ncomblevels) {
      if (tclvalue(withinSelection[[i]]) == gettext("--Choose one--", domain="R-RcmdrPlugin.aRnova")) {
        errorCondition(recall = repMeasAnova, 
                       message = gettext("You must select one measure per variable.", 
                                         domain="R-RcmdrPlugin.aRnova"))
        return()
      }
    }
    .activeDataSet <- ActiveDataSet()

   
    logger("# REMOVES INCOMPLETE CASES")
    
    compDS <- as.name(paste(substitute(.activeDataSet), ".cplt", sep=""))
    doItAndPrint(paste(compDS, " <- ", .activeDataSet, sep=""))
    
    doItAndPrint(paste("attach(", compDS,")", sep="")) 
    doItAndPrint(paste(compDS, " <- ", compDS, "[complete.cases(", 
                       repmeasures_list,"), ]", sep=""))    
    
    logger("# FORMATS TO 'LONG' FORMAT (cplt.lg extension)")
    
    longDS <- as.name(paste(substitute(compDS), ".lg",  sep=""))
    
    doItAndPrint(paste(longDS,  " <- reshape(data = ", compDS, 
                       ", varying = c(", repmeasures_listr, 
                       "), v.names = c(\"DV\"), timevar = c(\"trial\"), 
                       direction = \"long\")", sep=""))
    
    doItAndPrint(paste(longDS, " <- ", longDS, "[ order(", longDS, "[\"id\"], ", 
                       longDS, "[\"trial\"]), ]", sep=""))
    
    doItAndPrint(paste(longDS,"$id <- factor(",longDS,"$id)", sep=""))
    
    # adds within-subject variables as factors  
    
    k <- 1
    for (i in nfactor:1) {
      doItAndPrint(paste(longDS, "[, \"", .withinfactors[[i]],"\"] <- gl(",
                         .withinlevels[i], ", ", k, " , length = nrow(",
                         longDS, "))", sep=""))
      k <- k * .withinlevels[i]
    }
    
    ## add all interaction factors as factors
    predictors = .withinfactors  
    if (length(betweenfactors > 0)){
      predictors = c(betweenfactors, .withinfactors)
      }
    
    npredictors <- length(predictors)
    combfactors <- lapply(predictors, list)    # this will be the list of all combinations of factors (used for summary)
    
    doItAndPrint(paste("attach(", longDS, ")", sep=""))
    if (npredictors>1){
      for (i in 2:npredictors){
        comb_i <- combn(predictors, i)
        for (j in 1:ncol(comb_i)) {
          colname <- paste(paste(comb_i[, j], sep = ""), 
                           collapse = ".")
          comb_list <- paste(paste(comb_i[, j], sep = ""), collapse = ", ")
          combfactors[[length(combfactors)+1]] <- comb_i[, j]
         doItAndPrint(paste(longDS, "[,\"", colname, "\"] <- interaction(", 
                            comb_list,")", sep=""))
        }
      }
    }

    logger("# COMPUTE ANOVA")
    #### code for type I ANOVA Reapeated-Measures using aov{stats})                                 
    #     
    #   withincrosslist  = paste (paste(.withinfactors, sep = " "),
    #                             collapse = "*")
    #   betweencrosslist = paste (paste(betweenfactors, sep = " "),
    #                             collapse = "*")
    #   
    #   predictors_list = withincrosslist
    #   if (length(betweenfactors > 0)) {
    #       predictors_list = paste(withincrosslist, betweencrosslist, sep = "*")
    #   }
    #   
    #   Error_denom_list = paste(paste(.withinfactors, sep = " "), collapse = "*")
    #   
    #   doItAndPrint(paste("summary(aov(DV ~ ", predictors_list,"
    #                      + Error(id / ", Error_denom_list,
    #                      "), data = ", longDS, "))", sep = ""))
    ####   

    #### code for type II or III ANOVA Repeated-Measures using ezANOVA{ez}.
    #### requires adding ez in DESCRIPTION/Import field and import(ez) in NAMESPACE 
    # 
    #   if (length(betweenfactors) > 0) {
    #     doItAndPrint(paste(".myEzAnova <- ez::ezANOVA(data = ", longDS, 
    #                        ", dv = .(DV), wid = .(id), within = .(", 
    #                        withinfactors_list,"), between = .(", 
    #                        betweenfactors_list,
    #                        "), type = ", anova.ez.type, 
    #                        ", detailed = T, return_aov = T)", 
    #                        sep = ""))
    #   } else {
    #     doItAndPrint(paste(".myEzAnova <- ez::ezANOVA(data = ", longDS, 
    #                        ", dv = .(DV), wid = .(id), within = .(", 
    #                        withinfactors_list,
    #                        "), detailed = T, type = ", anova.ez.type, 
    #                        ", return_aov = T)", 
    #                        sep = ""))
    #   }
    #   
    #   doItAndPrint(paste(".myEzAnova$ANOVA$p <- format.pval(.myEzAnova$ANOVA$p, digits = 4, eps = 1e-3)", sep=""))
    #   doItAndPrint(paste(".myEzAnova$ANOVA$ges <- round(as.numeric(.myEzAnova$ANOVA$ges), 4)", sep=""))
    #   doItAndPrint(paste("print(".myEzAnova$ANOVA)", sep=""))
    #   justDoIt(paste(modelValue, " <- ", modelValue,"$aov", sep=""))
    ####  
    
    doItAndPrint(paste(".idata <- data.frame(matrix(nrow = ", 
                       prod(.withinlevels), ", ncol = 0))", sep=""))
    k <- 1
    for (i in nfactor:1) {
      doItAndPrint(paste(".idata <- data.frame( gl(",
                         .withinlevels[i], ", ", k, " , length = ",
                         prod(.withinlevels), "), .idata)", sep=""))
      k <- k * .withinlevels[i]
    }
   doItAndPrint(paste("colnames(.idata) <- c(", withinfactors_listr, ")", sep = ))
   withincrosslist  = paste (paste(.withinfactors, sep = " "),
                              collapse = "*")
   doItAndPrint(paste(".idesign <- ~ ", withincrosslist, sep=""))
   betweencrosslist = paste (paste(betweenfactors, sep = " "),
                             collapse = "*")
   doItAndPrint(paste("options(contrasts = c('contr.sum', 'contr.poly'))", sep=""))
  if (length(betweenfactors) > 0)  {
    doItAndPrint(paste(modelValue, " <- lm(cbind(", repmeasures_list,") ~ ", 
                       betweencrosslist,", data = ", compDS, ")", sep=""))
   } else {
    doItAndPrint(paste(modelValue, " <- lm(cbind(", repmeasures_list,
                       ") ~ 1, data = ",
                       compDS, ")", sep=""))
   }
   doItAndPrint(paste(".myAnova <- Anova(", modelValue,
                      ", idata = .idata, idesign = .idesign, type = ",
                      SStypeVar ,")" , sep=""))

   # show anova summary
   doItAndPrint(paste("summary(.myAnova, multivariate=FALSE)", sep=""))
   # reset contrasts 
   doItAndPrint(paste("options(contrasts = c('contr.treatment', 'contr.poly'))", sep=""))

   if (effectSizeVar == 1) {
   logger("# COMPUTE EFFECT SIZE")
     command <- character(4)
     command[1] <- paste(".effectSize <- data.frame(row.names = .myAnova$terms)", sep="")
     command[2] <- paste("for (.term in .myAnova$terms)", sep="")                    
     command[3] <- paste("  .effectSize[.term,\"part. eta sq.\"] <- .myAnova$SSP[[.term]] / (.myAnova$SSP[[.term]] + .myAnova$SSPE[[.term]]) ", sep="")
     command[4] <- paste("format(round(.effectSize, 4), nsmall = 4)")
     doItAndPrint(paste(command, collapse="\n"))
   }
   
   if (groupSummaryVar == 1) {
     logger("# NUMERIC SUMMARY FOR GROUPS")
     # generates a summary for groups defined by all combinations of factors 
     # ('combfactors') of within- and between subject factors
     
     for (i in 1:length(combfactors)) {
       pluslist <- paste (paste(combfactors[[i]], sep = " "), 
                          collapse = " + ")  
       doItAndPrint(paste("aggregate( DV ~ ", pluslist, " , data = ", longDS,
                          ", function(x) round(c(count = length(x), 
                          min = min(x), max = max(x), mean = mean(x),
                          sd = sd(x)), 3))",
                          sep=""))
     }
   }
   
   if (mulCompVar == 1) { 
     logger("# POST HOC ANALYSIS FOR SIGNIFICANT (OR CLOSE TO...) EFFECTS")
     Library("multcomp")

     count <- 0
     for (term in eval(parse(text = paste(".myAnova$terms")))) {
       colname <- gsub(":", ".", term)
       group <- eval(parse(text = paste(longDS, "[[\"", colname, "\"]]", sep="")))
       if (length(levels(group))>2){
         # keep only significant or close to significant terms (p < 0.1):
         # computes p-values returned by Anova(), which unfortunately are not 
         # easily extractible... see: 
         # http://r.789695.n4.nabble.com/extracting-p-values-from-Anova-objects-from-the-car-library-td2335657.html
         # the following reproduces p-value calculation made in 
         # car:::print.Anova.mlm()
         SSP      <- eval(parse(text = paste(".myAnova$SSP[[term]]")))
         SSPE     <- eval(parse(text = paste(".myAnova$SSPE[[term]]")))
         P        <- eval(parse(text = paste(".myAnova$P[[term]]")))
         p        <- ncol(P)
         PtPinv   <- solve(t(P) %*% P)
         SS       <- sum(diag(SSP %*% PtPinv))
         Error_SS <- sum(diag(SSPE %*% PtPinv))
         num_Df   <- eval(parse(text = paste(".myAnova$df[term]")))  * p
         den_Df   <- eval(parse(text = paste(".myAnova$error.df")))  * p
         Fstat    <- (SS / num_Df) / (Error_SS / den_Df)
         pvalue   <- pf(Fstat, num_Df, den_Df, lower.tail = FALSE)
        
         if (pvalue < 0.1){
           doItAndPrint(paste("summary(glht(aov(DV ~ ", colname, ", data = ", longDS, 
                             "), linfct = mcp(", colname, " = \"Tukey\")))", sep=""))
           count <- count + 1 
         }
       }
     }
     if (count == 0){
       logger("No significant effect or interaction with more than two levels ... ")
     }
   }
   
   # activeModel(modelValue)
   # putRcmdr("modelWithSubset", FALSE)
   logger("# END OF REPEATED MEASURES ANOVA")  
   tcltk::tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "repMeasAnova",      model = TRUE, 
                     reset = ".repMeasAnovaReset", apply = "repMeasAnova" ) 
  
  # Data Tab
  tkgrid(labelRcmdr(modelFrame, 
                    text = (paste("1. ", 
                                  gettextRcmdr("Enter name for model: "), 
                                  sep="")), foreground="blue"), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  
  tkgrid(askwithin, sticky="w")
  tkgrid(WithinFactorsNames)
  tkgrid(witinfactornamesFrame, sticky = "w")
  for (i in 1:ncomblevels){
    tkgrid(texts[[i]], comboboxwithin[[i]])
    tkgrid.configure(texts[[i]], sticky="nw")
    tkgrid.configure(comboboxwithin[[i]], sticky="w", padx=2, pady=2 )
  }
  tkgrid(dataFrame, sticky="w")
  
  tkgrid(askbetween, sticky="w")
  tkgrid(askbetweenFrame, sticky="w")
  tkgrid(BetweenFactors, listbox_between)
  tkgrid.configure(BetweenFactors, sticky="nw")
  tkgrid.configure(listbox_between, sticky="w")
  tkgrid(dataFrame2, sticky="w")
  
  # Options Tab
  tkgrid(askSStype, SStypeButtonsFrame, sticky="nw")
  tkgrid(checkBoxFrame, sticky="w")
  tkgrid(opFrame, sticky="w")
  
  tkadd    <- tcltk::tkadd
  tkselect <- tcltk::tkselect
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, 
               tab.names=c("Data", "Options"))
}

.repMeasAnovaReset <- function () {
  
  defaults      <- list(initial.withinSelection = NULL, 
                        initial.betweenSelection = NULL, 
                        initial.SStype = "2", 
                        initial.checkBox = c("0", "0", "0"),
                        initial.tab = 0)
  
  putDialog("repMeasAnova", defaults)
  repMeasAnovaSetup()
}


#' One way ANOVA
#'
#' This is a modification of \code{Rcmdr::oneWayAnova()}
#' where supplementary options have  been added.
#' 
#' Options:  
#' \itemize{
#' \item{\code{'Effect size'}} {compute and prints effect size (partial eta squared)}
#' \item{\code{'Summary statistics for groups'}} {prints summary statistics for
#' groups formed by the beween subject factor}
#' \item{\code{'Pairwise comparisons of means'}} {performs post-hoc Tukey's HSD test.}
#' }  
#' @details On OK, the following operations are carried out:
#' \itemize{
#'  \item{} {Computes ANOVA using \code{\link{aov}}} 
#'  \item{} {Computes effect sizes (partial eta squared)}
#'  \item{} {Prints a summary of marginal statistics 
#'  (count, min, max, mean, ds)}
#'  \item{} {runs post-hoc analysis}
#' }
#' 
#' @return None
#'
#' @seealso \code{\link{aov}} for the computation of ANOVA

oneWayAnova_ <- function () {

  Library("abind")
  defaults <- list(initial.group = NULL, initial.response = NULL, 
                   initial.checkBox = c("0", "0", "0"),
                   initial.tab = 0)
  
  dialog.values <- getDialog("oneWayAnova_", defaults)
  
  tkadd <- tcltk::tkadd
  ttknotebook <- tcltk::ttknotebook
  initializeDialog(title = gettextRcmdr("One-Way Analysis of Variance"), 
                   use.tabs=TRUE)
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(dataTab)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(dataTab)
  groupBox <- variableListBox(dataFrame, Factors(), 
                              title = gettextRcmdr("Groups (pick one)"), 
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), 
                                 title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))

  opFrame <- tkframe(optionsTab)
  
  checkBoxes(window = opFrame, frame="checkBoxFrame", boxes=c("effectSize", "groupSummary", "mulComp"), 
             initialValues=dialog.values$initial.checkBox, 
             labels=gettext(c("Effect size", "Summary statistics for groups", "Pairwise comparisons of means"), 
                            domain="R-RcmdrPlugin.aRnova"))
  
  onOK <- function() {
    

   
    tkselect <- tcltk::tkselect
	  tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = oneWayAnova_, 
                     message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                             modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        oneWayAnova_()
        return()
      }
    }
    group <- getSelection(groupBox)
    response <- getSelection(responseBox)
    closeDialog()
    if (length(group) == 0) {
      errorCondition(recall = oneWayAnova_, 
                     message = gettextRcmdr("You must select a groups factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = oneWayAnova_, 
                     message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    
    effectSizeVar   <- tclvalue(effectSizeVariable)
    groupSummaryVar <- tclvalue(groupSummaryVariable)
    mulCompVar      <- tclvalue(mulCompVariable)

    putDialog ("oneWayAnova_", list (initial.group = group, initial.response = response, 
                                     initial.checkBox = c(effectSizeVar, groupSummaryVar, mulCompVar),
                                     initial.tab = tab))
    
    command <- paste(modelValue, " <- aov(", response, " ~ ", 
                     group, ", data=", .activeDataSet, ")", sep = "")
    justDoIt(command)
    logger(command)
    doItAndPrint(paste(".myAnova <- summary(", modelValue, ")", sep = ""))
    doItAndPrint(paste(".myAnova", sep = ""))
    
    #AT: this is added to report effect size
    if (effectSizeVar == 1){
      logger("# EFFECT SIZE")
      doItAndPrint(paste(".effectSize <- data.frame(format(round(.myAnova[[1]]$`Sum Sq` / sum(.myAnova[[1]]$\`Sum Sq\`), 4), nsmall = 4)", 
                         ", row.names = rownames(.myAnova[[1]]))", sep="")) 
      doItAndPrint(paste("colnames(.effectSize) <- c(\"part. eta sq.\")", sep = ""))
      doItAndPrint(paste(".effectSize"))
    }
    
    if (groupSummaryVar == 1){
      logger("# NUMERIC SUMMARY OF GROUPS")
      doItAndPrint(paste("with(", .activeDataSet, ", numSummary(",
                         response, ", groups=", group, 
                         ", statistics=c(\"mean\", \"sd\")))", sep = ""))
      activeModel(modelValue)
      putRcmdr("modelWithSubset", FALSE)
      

      if (mulCompVar == 1) {
        if (eval(parse(text = paste("length(levels(", .activeDataSet, 
                                    "$", group, ")) < 3")))) 
          Message(message = gettextRcmdr("Factor has fewer than 3 levels; pairwise comparisons omitted."), 
                  type = "warning")
        else {
          Library("multcomp")
          commands <- character(7)
          commands[1] <- paste("local({\n  .Pairs <- glht(", modelValue, 
                               ", linfct = mcp(", group, " = \"Tukey\"))", 
                               sep = "")
          commands[2] <- "  print(summary(.Pairs)) # pairwise tests"
          commands[3] <- "  print(confint(.Pairs)) # confidence intervals"
          commands[4] <- "  print(cld(.Pairs)) # compact letter display"
          commands[5] <- "  old.oma <- par(oma=c(0,5,0,0))"
          commands[6] <- "  plot(confint(.Pairs))"
          commands[7] <- "  par(old.oma)\n})"
          doItAndPrint(paste(commands, collapse="\n"))
        }
      }
    }
    tkfocus(CommanderWindow())
  }
    
  OKCancelHelp(helpSubject = "oneWayAnova_", model = TRUE, reset = "oneWayAnova_", 
               apply = "oneWayAnova_")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w", columnspan = 2)
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), 
         getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  
  tkgrid(checkBoxFrame, sticky="w")
  tkgrid(opFrame, sticky="w")
  
  tkadd <- tcltk::tkadd
  tkselect <- tcltk::tkselect
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, 
               tab.names=c("Data", "Options"))
}


#' Multiway ANOVA
#'
#' This is a modification of \code{Rcmdr::multiWayAnova()}
#' where supplementary options have  been added.
#' 
#' @details 
#' Options:  
#' \itemize{
#' \item{\code{'SS type'}} {type of sum of squared, default: \code{type = 2}. 
#' See Details in \code{\link[car]{Anova}}}
#' \item{\code{'Effect size'}} {compute and prints effect size (partial eta squares)}
#' \item{\code{'Summary statistics for groups'}} {prints summary statistics for
#' groups formed by all combinatuions of factors}
#' \item{\code{'Pairwise comparisons of means'}} {performs post-hoc Tukey's HSD test  
#' on significant (p < .05) or close to significant (p < 0.1) effects.}
#' }  
#' On OK, the following operations are carried out:
#' \itemize{
#'  \item{} {Computes ANOVA using \code{\link[car]{Anova}}} 
#'  \item{} {Computes effect sizes (partial eta squared)}
#'  \item{} {Prints a summary of marginal statistics 
#'  (count, min, max, mean, ds)}
#'  \item{} {runs post-hoc analysis on significant or close to significant effects}
#'  \item {} {Generates an 'extended' dataset (extension  \code{.ext}) containing
#'  additionak columns \code{'<factorA.factorB:...>'} that allows differentiate
#'  measures from groups or subjects with same factors levels.
#'  This 'extended' dataset is useful for ploting means and post-hoc analysis  
#'  }
#' }
#' 
#' @return None
#'
#' @seealso \code{\link[car]{Anova}} for the computation of ANOVA

multiWayAnova_ <- function () {
  
  Library("multcomp")
  
  defaults <- list(initial.group = NULL, initial.response = NULL,
                   initial.SStype = "2", 
                   initial.checkBox = c("0", "0", "0"),
                   initial.tab = 0)
  
  dialog.values <- getDialog("multiWayAnova_", defaults)
  
  tkadd <- tcltk::tkadd
  ttknotebook <- tcltk::ttknotebook
  
  initializeDialog(title = gettextRcmdr("Multi-Way Analysis of Variance"), 
                   use.tabs=TRUE)
  
  # Data Tab
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(dataTab)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(dataTab)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple", 
                              title = gettextRcmdr("Factors (pick one or more)"), 
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), 
                                 title = gettextRcmdr("Response Variable (pick one)"), 
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  
  # Options Tab
  opFrame <- tkframe(optionsTab)
  askSStype <- tklabel(opFrame, 
                       textvariable = 
                         tclVar(gettext("Sum of Squares type", 
                                        domain="R-RcmdrPlugin.aRnova")))
  
  radioButtons(window = opFrame, name="SStypeButtons", buttons=c("b2", "b3"), 
               values=c("2", "3"), 
               initialValue=dialog.values$initial.SStype,
               labels=gettext(c("Type 2", "Type 3"), 
                              domain="R-RcmdrPlugin.aRnova"))
  
  checkBoxes(window = opFrame, frame="checkBoxFrame", boxes=c("effectSize", "groupSummary", "mulComp"), 
             initialValues=dialog.values$initial.checkBox, 
             labels=gettext(c("Effect size", "Summary statistics for groups", "Pairwise comparisons of means"), domain="R-RcmdrPlugin.aRnova"))
  
  onOK <- function() {
  
  tkselect <- tcltk::tkselect
	tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = multiWayAnova_, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                               modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        tkdestroy(dataTab)
        tkdestroy(optionsTab)
        multiWayAnova_()
        return()
      }
    }
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    
    # AT: get options
    SStypeVar       <- tclvalue(SStypeButtonsVariable)     
    effectSizeVar   <- tclvalue(effectSizeVariable)
    groupSummaryVar <- tclvalue(groupSummaryVariable)
    mulCompVar      <- tclvalue(mulCompVariable)
    
    putDialog ("multiWayAnova_", list (initial.group = groups, initial.response = response,
                                       initial.SStype = SStypeVar, 
                                       initial.checkBox = c(effectSizeVar, groupSummaryVar, mulCompVar),
                                       initial.tab = tab))
    closeDialog()
    if (length(groups) == 0) {
      errorCondition(recall = multiWayAnova_, message = gettextRcmdr("You must select at least one factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = multiWayAnova_, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    
    
    # AT: change contrasts (default OK for type 2 but not type 3 SS) 
    doItAndPrint(paste("options(contrasts = c('contr.sum', 'contr.poly'))", sep=""))
    groups.list <- paste(paste(groups, sep = ""), collapse = ", ")
    doItAndPrint(paste(modelValue, " <- lm(", response, 
                       " ~ ", paste(groups, collapse = "*"), ", data=", 
                       .activeDataSet, ")", sep = ""))
    doItAndPrint(paste(".myAnova <- Anova(", modelValue, ", type = ", SStypeVar,")", sep = ""))
    doItAndPrint(paste(".myAnova", sep = ""))
    # AT; reset contrasts
    doItAndPrint(paste("options(contrasts = c('contr.treatment', 'contr.poly'))", sep="")) 
    #AT: this is added to report effect size
    if (effectSizeVar == 1){
      logger("# EFFECT SIZE")
      doItAndPrint(paste(".effectSize <- data.frame(format(round(.myAnova$`Sum Sq` / sum(.myAnova$\`Sum Sq\`), 4), nsmall = 4)", 
                         ", row.names = rownames(.myAnova))", sep="")) 
      doItAndPrint(paste("colnames(.effectSize) <- c(\"part. eta sq.\")", sep = ""))
      doItAndPrint(paste(".effectSize"))
    }
    if (groupSummaryVar == 1){
      logger("# NUMERIC SUMMARY OF GROUPS")
      doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response, 
                         ", list(", groups.list, "), mean, na.rm=TRUE))) # means", 
                         sep = ""))
      doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response, 
                         ", list(", groups.list, "), sd, na.rm=TRUE))) # std. deviations", 
                         sep = ""))
      doItAndPrint(paste("with(", .activeDataSet, ", (tapply(", response, 
                         ", list(", groups.list, "), function(x) sum(!is.na(x))))) # counts", 
                         sep = ""))
    }
    if (mulCompVar == 1){
      logger("# POST HOC ANALYSIS FOR SIGNIFICANT (OR CLOSE TO...) EFFECTS")
      count <- 0
      
      # makes an extended dataset including interactions between factors
      extDS <- as.name(paste(substitute(.activeDataSet), ".extd", sep=""))
      doItAndPrint(paste(extDS, " <- ", .activeDataSet, sep=""))
      
      ## add all interaction factors as factors
      
      nfactors <- length(groups)
      # combfactors <- lapply(groups, list)    # list of all combinations of factors (used for summary)
      
      doItAndPrint(paste("attach(", extDS, ")", sep=""))
      if (nfactors>1){
        for (i in 2:nfactors){
          comb_i <- combn(groups, i)
          for (j in 1:ncol(comb_i)) {
            colname <- paste(paste(comb_i[, j], sep = ""), 
                             collapse = ".")
            comb_list <- paste(paste(comb_i[, j], sep = ""), collapse = ", ")
            # combfactors[[length(combfactors)+1]] <- comb_i[, j]
            doItAndPrint(paste(extDS, "[,\"", colname, "\"] <- interaction(", 
                               comb_list,")", sep=""))
          }
        }
      }
       
      for (term in eval(parse(text = paste("rownames(.myAnova)[2:(nrow(.myAnova)-1)]")))) {
        colname <- gsub(":", ".", term)
        group <- eval(parse(text = paste(extDS, "[[\"", colname, "\"]]", sep="")))
        if (length(levels(group))>2){
          # keep only significant or close to significant terms (p < 0.1):
          pvalue = .myAnova[term, 'Pr(>F)']
          if (pvalue < 0.1){
            doItAndPrint(paste("summary(glht(aov(", response," ~ ", colname, ", data = ", extDS, 
                               "), linfct = mcp(", colname, " = \"Tukey\")))", sep=""))
            count <- count + 1 
          }
        }
      }
      if (count == 0){
        logger("No significant effect or interaction with more than two levels ... ")
      }
    }
    
    logger("# END OF MULTIWAY ANOVA ANALYSIS")
    activeModel(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "multiWayAnova_", model = TRUE, reset = "multiWayAnova_", apply = "multiWayAnova_")
  # Data Tab
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  
  # Options Tab
  tkgrid(askSStype, SStypeButtonsFrame, sticky="nw")
  tkgrid(checkBoxFrame, sticky="w")
  tkgrid(opFrame, sticky="w")
  
  tkadd <- tcltk::tkadd
  tkselect <- tcltk::tkselect
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE, 
               tab.names=c("Data", "Options"))
}

#' Generalized Linear Model
#' 
#' This is a minor modification of \code{\link[Rcmdr]{generalizedLinearModel}}
#' where size effects are computed and displayed for logistic regression
#' 
#' @seealso \code{\link[Rcmdr]{generalizedLinearModel}} 

generalizedLinearModel_ <- function(){

  families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
                "quasibinomial", "quasipoisson")
  links <- c("identity", "inverse", "log", "logit", "probit",
             "cloglog", "sqrt", "1/mu^2")
  availableLinks <- matrix(c(
    TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
    TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
    TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
    TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
    FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
    TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
    7, 8, byrow=TRUE)
  rownames(availableLinks) <- families
  colnames(availableLinks) <- links
  canonicalLinks <- c("identity", "logit", "log", "inverse", "1/mu^2", "logit", "log")
  names(canonicalLinks) <- families
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"))
  dialog.values <- getDialog("generalizedLinearModel_", defaults)
  initializeDialog(title=gettextRcmdr("Generalized Linear Model"))
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "glm"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), glm=TRUE)
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  modelFormula()
  UpdateModelNumber()
  modelName <- tclVar(paste("GLM.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  linkFamilyFrame <- tkframe(top)
  familyFrame <- tkframe(linkFamilyFrame)
  max.height <- getRcmdr("variable.list.height")
  familyBox <- tklistbox(familyFrame, height=min(max.height, length(families)), 
                         exportselection="FALSE",
                         selectmode="single", background="white")
  familyScroll <- ttkscrollbar(familyFrame,
                               command=function(...) tkyview(familyBox, ...))
  tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
  for (fam in families) tkinsert(familyBox, "end", fam)
  linkFrame <- tkframe(linkFamilyFrame)
  linkBox <- tklistbox(linkFrame, height=max.height, exportselection="FALSE",
                       selectmode="single", background="white")
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  onFamilySelect <- function(){
    family <- families[as.numeric(tkcurselection(familyBox)) + 1]
    availLinks <- links[availableLinks[family,]]
    tkdelete(linkBox, "0", "end")
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    canLink <- canonicalLinks[family]
    tkconfigure(linkBox, height=length(availLinks))
    tkselection.set(linkBox, which(canLink == availLinks) - 1)
  }
  onOK <- function(){
    check.empty <- gsub(" ", "", tclvalue(lhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=generalizedLinearModel_, model=TRUE, message=gettextRcmdr("Left-hand side of model empty."))
      return()
    }
    check.empty <- gsub(" ", "", tclvalue(rhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=generalizedLinearModel_, model=TRUE, message=gettextRcmdr("Right-hand side of model empty."))
      return()
    }
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      errorCondition(recall=generalizedLinearModel_, model=TRUE, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
    }
    if (is.element(modelValue, listGeneralizedLinearModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
        UpdateModelNumber(-1)
        closeDialog()
        generalizedLinearModel_()
        return()
      }
    }
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    family <- families[as.numeric(tkcurselection(familyBox)) + 1]
    availLinks <- links[availableLinks[family,]]
    link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
    subset <- tclvalue(subsetVariable)
    closeDialog()
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    }
    else{
      subset <- paste(", subset=", subset, sep="")
      putRcmdr("modelWithSubset", TRUE)
    }
    weight.var <- getSelection(weightComboBox)
    putDialog("generalizedLinearModel_", list(initial.weight = weight.var))
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else paste(", weights=", weight.var, sep="")
    command <- paste("glm(", formula, ", family=", family, "(", link,
                     "), data=", ActiveDataSet(), subset, weights, ")", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if ((family == "binomial" || family =="quasibinomial") && link == "logit"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients ("odds ratios")'))
      
    # AT: the following has been added do compute and display size effects
    logger("# COMPUTES SIZE EFFECT ")
    
    doItAndPrint(paste(".modelChisq <- ", modelValue, "$null.deviance - ", modelValue, "$deviance", sep=""))
    doItAndPrint(paste(".Chisqdf <- ", modelValue, "$df.null - ", modelValue, "$df.residual", sep=""))
    doItAndPrint(paste(".Chisq.prob <- 1 - pchisq(.modelChisq, .Chisqdf)", sep=""))
    doItAndPrint(paste(".R2.hl <- .modelChisq / ", modelValue,"$null.deviance", sep=""))
    doItAndPrint(paste(".N <- length(", modelValue, "$y)", sep=""))
    doItAndPrint(paste(".R2.cs <- 1-exp((", modelValue, "$deviance - ", modelValue, "$null.deviance)/.N)", sep=""))
    doItAndPrint(paste(".R2.n <- .R2.cs / (1 - (exp(-(", modelValue, "$null.deviance)/.N))) ", sep=""))
    
    doItAndPrint(paste(".effectSize <- data.frame(c(.modelChisq, .Chisqdf, .Chisq.prob, .R2.hl, .R2.cs, .R2.n), row.names=c(\"Chi2\", \"df\", \"p-value\", \"Hosmer-Lemeshow\", \"Cox Snell  R2\", \"Nagelkerke R2\"))", sep="" ))
    doItAndPrint(paste("colnames(.effectSize) <- \"Effect Size\"", sep=""))  
    doItAndPrint(paste("format(round(.effectSize, 5), nsmall = 5)"))
    
    logger("# END OF LOGISTIC REGRESSION")
    }
    if ((family == "poisson" || family =="quasipoisson") && link == "log"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients'))
    }
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="generalizedLinearModel_", model=TRUE, reset="resetGLM", apply="generalizedLinearModel_")
  helpButton <- buttonRcmdr(buttonsFrame, text="Help", width="12", command=onHelp)
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(subsetWeightFrame, sticky="w")  
  tkgrid(labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Family (double-click to select)"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
         labelRcmdr(linkFamilyFrame, text="   "), labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Link function"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(familyBox, familyScroll, sticky="nw")
  tkgrid(linkBox, sticky="nw")
  tkgrid(familyFrame, labelRcmdr(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
  tkgrid(linkFamilyFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  tkgrid.configure(familyScroll, sticky="ns")
  fam <- if (currentModel) which(currentFields$family == families) - 1
  else 1
  tkselection.set(familyBox, fam)
  availLinks <- links[availableLinks[fam + 1,]]
  for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
  tkconfigure(linkBox, height=length(availLinks))
  lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
  else 0
  tkselection.set(linkBox, lnk)
  tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}
