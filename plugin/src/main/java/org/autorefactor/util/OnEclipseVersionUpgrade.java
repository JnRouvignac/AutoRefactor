package org.autorefactor.util;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/** To easily find code to upgrade on upgrading the Eclipse baseline. */
@Retention(SOURCE)
@Target({ ANNOTATION_TYPE, CONSTRUCTOR, FIELD, LOCAL_VARIABLE, METHOD, PACKAGE, PARAMETER, TYPE })
public @interface OnEclipseVersionUpgrade {
    /** Descriptions of what code changes should be performed on upgrading the Eclipse baseline. */
    String[] value();
}
