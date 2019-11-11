package org.autorefactor.jdt.internal.corext.dom;

import org.eclipse.core.runtime.jobs.Job;

/**
 * Abstract super class of all refactoring jobs, marking them as belonging to a certain family, thereby simplifying the cancellation of multiple jobs.
 *
 */
public abstract class AbstractRefactoringJob extends Job {

	public AbstractRefactoringJob(String name) {
		super(name);
	}

	/**
	 * all instances of this job belong to this family
	 */
	public static final Object FAMILY = new Object();
	
    @Override
    public final boolean belongsTo(Object family) {
    	if (FAMILY == family) {
    		return true;
    	}
    	return super.belongsTo(family);
    }

}
