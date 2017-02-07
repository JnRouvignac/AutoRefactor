package org.autorefactor.refactoring.rules.samples_in;

import java.util.logging.Logger;

import org.apache.logging.log4j.LogManager;
import org.slf4j.LoggerFactory;

public class EnforceStaticFinalLoggerSample {

    public Logger LOG = Logger.getLogger("publicLogger");

    Logger LOG2 = Logger.getLogger("package protected");

    protected Logger LOG3 = Logger.getLogger("protected");

    private Logger LOG4 = Logger.getLogger("private Logger");

    Logger instanceLogger;

    private org.apache.logging.log4j.Logger log4jLogger = LogManager.getLogger(EnforceStaticFinalLoggerSample.class);

    static org.slf4j.Logger slf4jLogger = LoggerFactory.getLogger(EnforceStaticFinalLoggerSample.class);

    public EnforceStaticFinalLoggerSample(Logger l) {
        this.instanceLogger = l;
    }

}
