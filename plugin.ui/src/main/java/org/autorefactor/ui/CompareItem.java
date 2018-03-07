package org.autorefactor.ui;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;

import org.eclipse.compare.IEditableContent;
import org.eclipse.compare.IModificationDate;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;

public class CompareItem implements IStreamContentAccessor, ITypedElement, IModificationDate, IEditableContent {
    private String content;
    private File fileContent;
    static String UNKNOWN_TYPE = "JAVA";
    public static String newContents;
    public static String newFileCnt;

    public CompareItem(String left) {
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
        return UNKNOWN_TYPE;
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
        // TODO Auto-generated method stub
        try {
            newContents = new String(newContent, "UTF-8");
        } catch (Exception e) {
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