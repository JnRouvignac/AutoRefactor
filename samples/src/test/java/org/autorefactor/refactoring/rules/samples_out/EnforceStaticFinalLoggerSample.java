package org.autorefactor.refactoring.rules.samples_out;

import java.util.logging.Logger;

import org.apache.logging.log4j.LogManager;
import org.slf4j.LoggerFactory;

public class EnforceStaticFinalLoggerSample {

    private static final Logger LOG = Logger.getLogger("publicLogger");

    private static final Logger LOG2 = Logger.getLogger("package protected");

    protected static final Logger LOG3 = Logger.getLogger("protected");

    private static final Logger LOG4 = Logger.getLogger("private Logger");

    Logger instanceLogger;

    private static final org.apache.logging.log4j.Logger log4jLogger = LogManager.getLogger(EnforceStaticFinalLoggerSample.class);

    private static final org.slf4j.Logger slf4jLogger = LoggerFactory.getLogger(EnforceStaticFinalLoggerSample.class);

    public EnforceStaticFinalLoggerSample(Logger l) {
        this.instanceLogger = l;
    }

}
