package org.autorefactor.ui;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.eclipse.compare.ICompareNavigator;
import org.eclipse.compare.IEditableContent;
import org.eclipse.compare.IModificationDate;
import org.eclipse.compare.INavigatable;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.compare.ResourceNode;

public class CompareItem implements  IStreamContentAccessor, ITypedElement, IModificationDate, IEditableContent{

	private String content;
	static String TEXT_TYPE = "JAVA";
	public static String newContents;
	
	public CompareItem(String left ) {
		
		content = left;
		
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
		return TEXT_TYPE;
	}

	@Override
	public InputStream getContents() throws CoreException {
		// TODO Auto-generated method stub
		
		return new ByteArrayInputStream(content.getBytes());
	}

	@Override
	public boolean isEditable() {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public void setContent(byte[] newContent) {
		getType();
		// TODO Auto-generated method stub
		try{
			newContents = new String(newContent, "UTF-8");
		}
		catch(Exception e) {
			System.out.println("Set Content failed");
			e.printStackTrace();
		}
		
	}

	public static String getNewContent() {
		return newContents;
	}
	
	@Override
	public ITypedElement replace(ITypedElement dest, ITypedElement src) {
		
		// TODO Auto-generated method stub
		return null;
	}


	
	

}