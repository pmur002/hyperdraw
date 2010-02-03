

####################
# A graphBPH is a graphNEL with certain additional constraints

# The nodes of a graphBPH are divided into two sets:
# normal nodes and "edge nodes"

# The 'edgeNodePattern' is a regular expression that is
# used to define the two node sets (based on the node names)

# The intention is that a graphBPH may not be modified once
# it has been created


setClass("graphBPH",
         representation(# Specified by user
                        graph="graphNEL",
                        edgeNodePattern="character",
                        # Derived from user-spec
                        nodes="character",
                        edgeNodes="character",
                        edgeNodeIO="list"))
                        

####################
# An RagraphBPH is an Ragraph with certain additional constraints

setClass("RagraphBPH",
         representation(graph="Ragraph",
                        allNodes="character",
                        nodes="character",
                        edgeNodes="character",
                        edgeNodeIO="list"))
                        
