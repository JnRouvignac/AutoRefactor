/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java "Failed bases.
 *
 * Copyright (C) 2019 Fabrice Tiercelin - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_out;

import org.slf4j.Logger;

public class LogParametersRatherThanLogMessageSample {
    public void replaceConcatenationByParameter(Logger slf4jLog, int returnCode) {
        // Keep this comment
        slf4jLog.debug("Processed with return code : {}", returnCode);
        slf4jLog.error("Processed with return code : {}", returnCode);
        slf4jLog.info("Processed with return code : {}", returnCode);
        slf4jLog.trace("Processed with return code : {}", returnCode);
        slf4jLog.warn("Processed with return code : {}", returnCode);
    }

    public void replaceConcatenationUsingLogback(ch.qos.logback.classic.Logger logbackLog, int returnCode) {
        // Keep this comment
        logbackLog.debug("Processed with return code : {}", returnCode);
        logbackLog.error("Processed with return code : {}", returnCode);
        logbackLog.info("Processed with return code : {}", returnCode);
        logbackLog.trace("Processed with return code : {}", returnCode);
        logbackLog.warn("Processed with return code : {}", returnCode);
    }

    public void replaceConcatenationByParameters(Logger slf4jLog, String processName, int returnCode) {
        // Keep this comment
        slf4jLog.debug("Processed {} with return code : {}", processName, returnCode);
        slf4jLog.error("Processed {} with return code : {}", processName, returnCode);
        slf4jLog.info("Processed {} with return code : {}", processName, returnCode);
        slf4jLog.trace("Processed {} with return code : {}", processName, returnCode);
        slf4jLog.warn("Processed {} with return code : {}", processName, returnCode);
    }

    public void replaceConcatenationStartingWithParameter(Logger slf4jLog, String processName, int returnCode) {
        // Keep this comment
        slf4jLog.debug("{} processed with return code : {}", processName, returnCode);
        slf4jLog.error("{} processed with return code : {}", processName, returnCode);
        slf4jLog.info("{} processed with return code : {}", processName, returnCode);
        slf4jLog.trace("{} processed with return code : {}", processName, returnCode);
        slf4jLog.warn("{} processed with return code : {}", processName, returnCode);
    }

    public void replaceConcatenationWithThrowable(Logger slf4jLog, Throwable error) {
        // Keep this comment
        slf4jLog.debug("Failed with error : {}", String.valueOf(error));
        slf4jLog.error("Failed with error : {}", String.valueOf(error));
        slf4jLog.info("Failed with error : {}", String.valueOf(error));
        slf4jLog.trace("Failed with error : {}", String.valueOf(error));
        slf4jLog.warn("Failed with error : {}", String.valueOf(error));
    }

    public void doNotReplaceConcatenationWithExistingParameter(Logger slf4jLog, String processName, int returnCode) {
        slf4jLog.debug("Processed " + processName + " with return code : {}", returnCode);
        slf4jLog.error("Processed " + processName + " with return code : {}", returnCode);
        slf4jLog.info("Processed " + processName + " with return code : {}", returnCode);
        slf4jLog.trace("Processed " + processName + " with return code : {}", returnCode);
        slf4jLog.warn("Processed " + processName + " with return code : {}", returnCode);
    }

    public void doNotReplaceConcatenationWithExistingBrackets(Logger slf4jLog, int returnCode) {
        slf4jLog.debug("{} processed with return code : " + returnCode);
        slf4jLog.error("{} processed with return code : " + returnCode);
        slf4jLog.info("{} processed with return code : " + returnCode);
        slf4jLog.trace("{} processed with return code : " + returnCode);
        slf4jLog.warn("{} processed with return code : " + returnCode);
    }

    public void doNotReplaceConcatenateSplitLiteral(Logger slf4jLog) {
        slf4jLog.debug("This text is very long and it can't be kept on a single line"
                + " so we have to break it in order to keep readability.");
    }
}
