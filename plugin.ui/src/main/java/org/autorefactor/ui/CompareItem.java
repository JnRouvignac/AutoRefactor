package org.autorefactor.ui;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.eclipse.compare.IModificationDate;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;

public class CompareItem  implements IStreamContentAccessor, ITypedElement, IModificationDate{

	private String content;
	public CompareItem(String left ) {
		super();
		content = left;
		// TODO Auto-generated constructor stub
	}

	@Override
	public long getModificationDate() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Image getImage() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public InputStream getContents() throws CoreException {
		// TODO Auto-generated method stub
		return new ByteArrayInputStream(content.getBytes());
	}
	

}