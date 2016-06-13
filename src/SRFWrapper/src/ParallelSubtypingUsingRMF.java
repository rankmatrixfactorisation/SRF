

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import srf.subtype.paropt.GreedyParallelSubtyping;

public class ParallelSubtypingUsingRMF {
	
	public static void main(String[] args) {

		Option opDifRankFile = OptionBuilder.withArgName("DifRankFile").hasArg().withDescription("Ranked diffusion file").create("df");
		Option opExpRankFile = OptionBuilder.withArgName("ExpRankFile").hasArg().withDescription("Ranked expression file").create("ef");
		Option opMutationFile = OptionBuilder.withArgName("MutationFile").hasArg().withDescription("Mutation file").create("mf");
		Option opInitFFile = OptionBuilder.withArgName("InitMatrixF").hasArg().withDescription("Initialized Matrix F").create("if");
        Option opWorkingDir = OptionBuilder.withArgName("WorkingDir").hasArg().withDescription("Working directory").create("dir");
        Option opK = OptionBuilder.withArgName("k").hasArg().withDescription("Number of pattern sets").create("k");
        Option opDifTheta = OptionBuilder.withArgName("dtheta").hasArg().withDescription("Theta threshold for diffusion").create("dtheta");
        Option opExpTheta = OptionBuilder.withArgName("etheta").hasArg().withDescription("Theta threshold for expression").create("etheta");
        Option opBeta = OptionBuilder.withArgName("beta").hasArg().withDescription("beta").create("beta");
        Option opReqMutations = OptionBuilder.withArgName("nReqMut").hasArg().withDescription("The required number of mutations").create("nReqMut");
        //
        Option opMaxDifRank = OptionBuilder.withArgName("maxD").hasArg().withDescription("maximal ranked diffusion").create("maxD");
        Option opMaxExpRank = OptionBuilder.withArgName("maxE").hasArg().withDescription("maximal ranked expression").create("maxE");
                
        Option opLog	= new Option("log", "Log intermediate results into files");
        Option opDebug	= new Option("debug", "Debug");                        
        Option help	= new Option("help", "Print help");      
                
        Options options = new Options();
        options.addOption(opDifRankFile);
        options.addOption(opExpRankFile);
        options.addOption(opMutationFile);
        options.addOption(opInitFFile);
        options.addOption(opWorkingDir);
        options.addOption(opK);        
        options.addOption(opDifTheta);
        options.addOption(opExpTheta);
        options.addOption(opBeta);
        options.addOption(opMaxDifRank);
        options.addOption(opMaxExpRank);        
        
        options.addOption(help);
        options.addOption(opLog);
        options.addOption(opDebug);
        options.addOption(opReqMutations);
        
        // default values                
        String difFileName = "" ;
        String expFileName = "";
        String mutFileName = "";
        String initFFileName = "";
        String workingDir	= "./";
      
        int k = 5;        
        double dtheta = 0.86;
        double etheta = 0.65;
        
        int maxDifRank = 0;
        int maxExpRank = 0;
        int beta = 1;        
        int nReqMutations = 2;
        
        boolean bLog = false;
        boolean bDebug = false;                                
        // parse command line to get values
        CommandLineParser parser = new BasicParser();
        
        try{
        	CommandLine cmd = parser.parse( options, args);
        	if (cmd.hasOption("df")) {
        	    difFileName = cmd.getOptionValue("df");
        	}
        	if (cmd.hasOption("ef")) {
        	    expFileName = cmd.getOptionValue("ef");
        	}
        	if (cmd.hasOption("mf")) {
        		mutFileName = cmd.getOptionValue("mf");
        	}
        	
        	if (cmd.hasOption("if")) {
        		initFFileName = cmd.getOptionValue("if");
        	}
        	
        	if (cmd.hasOption("dtheta")) {
        		dtheta = Double.parseDouble(cmd.getOptionValue("dtheta"));
        	}
        	
        	if (cmd.hasOption("etheta")) {
        		etheta = Double.parseDouble(cmd.getOptionValue("etheta"));
        	}
     	
        	
        	if (cmd.hasOption("beta")) {
        		beta = Integer.parseInt(cmd.getOptionValue("beta"));
        	}
        	      	
        	
        	if (cmd.hasOption("dir")) {
        		workingDir = cmd.getOptionValue("dir");
        	}
        	
        	if (cmd.hasOption("k")) {
        		k = Integer.parseInt(cmd.getOptionValue("k"));
        	}
        	
        	if (cmd.hasOption("nReqMut")) {
        		nReqMutations = Integer.parseInt(cmd.getOptionValue("nReqMut"));
        	}
        	       	
        	
        	if (cmd.hasOption("maxD")) {
        		maxDifRank = Integer.parseInt(cmd.getOptionValue("maxD"));
        	}
        	
        	if (cmd.hasOption("maxE")) {
        		maxExpRank = Integer.parseInt(cmd.getOptionValue("maxE"));
        	}
        	
        	if (cmd.hasOption("log")) {
        		bLog = true;
        	}
        	
        	if (cmd.hasOption("debug")) {
        		bDebug = true;
        	}
        	
        	
        	if (cmd.hasOption("help")) {
        		HelpFormatter formatter = new HelpFormatter();
        		formatter.printHelp( "Subtyping using rank matrix factorisation - parallel version", options );
        		return;
        	}
        	
        	
        }catch (ParseException ex){
        	System.out.println( ex.getMessage());
        }
        
        System.out.print("Working directory = "); System.out.println(workingDir);
        System.out.println("Ranked diffusion file = " + difFileName);
        System.out.println("Ranked expression file = " + expFileName);
        System.out.println("Mutation file = " + mutFileName);
        System.out.println("Initialised matrix F file = " + initFFileName); 
        System.out.print("dtheta = "); System.out.println(dtheta);
        System.out.print("etheta = "); System.out.println(etheta);
        System.out.print("beta = "); System.out.println(beta);
        System.out.print("Number of required mutations = "); System.out.println(nReqMutations);
        System.out.print("maxDifRank = "); System.out.println(maxDifRank);
        System.out.print("maxExpRank = "); System.out.println(maxExpRank);
        System.out.print("Number of patterns = "); System.out.println(k);
        System.out.print("Logging = "); System.out.println(bLog);
        System.out.print("Debug = "); System.out.println(bDebug);
        
        if (difFileName.isEmpty()) {
        	System.out.println("Diffusion file is not provided. Stop.");
        	return;
        }
        
        if (expFileName.isEmpty() ) {
        	System.out.println("Expression file is not provided. Stop.");
        	return;
        }
        
        if (mutFileName.isEmpty()) {
        	System.out.println("Mutation file is not provided. Stop.");
        	return;
        }
        
        if (initFFileName.isEmpty()) {
        	System.out.println("Initialised matrix F file is not provided. Stop.");
        	return;
        }
        
        // saving the command line
        String cmdLine = "";
        for (String arg: args) {
        	cmdLine +=  arg + " ";
        }
        srf.log.Logger logger = new srf.log.Logger();
        logger.log(cmdLine, workingDir + "cmdline.txt", false);
        
        GreedyParallelSubtyping miner = new GreedyParallelSubtyping(mutFileName, 
													difFileName, 
													expFileName,
													initFFileName,
													maxDifRank,		// max ranked diffusion value 
													maxExpRank,
													nReqMutations,
													k, 
													dtheta, 
													etheta,
													beta,													
													workingDir,
													bLog, 
													bDebug);
       miner.repeatDecomposition(1);
	}

}
