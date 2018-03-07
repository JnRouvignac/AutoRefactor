package org.autorefactor.ui;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.eclipse.compare.IEditableContent;
import org.eclipse.compare.IModificationDate;
import org.eclipse.compare.IStreamContentAccessor;
import org.eclipse.compare.ITypedElement;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;

/**
 * Compare item.
 */
public class CompareItem implements IStreamContentAccessor, ITypedElement, IModificationDate, IEditableContent {
    private String content;
    private static final String UNKNOWN_TYPE = "JAVA";
    private static String newContents;

    /**
     * Constructor.
     *
     * @param left the left.
     */
    public CompareItem(String left) {
        content = left;
    }

    @Override
    public long getModificationDate() {
        return 0;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public Image getImage() {
        return null;
    }

    @Override
    public String getType() {
        return UNKNOWN_TYPE;
    }

    @Override
    public InputStream getContents() throws CoreException {
        return new ByteArrayInputStream(content.getBytes());
    }

    @Override
    public boolean isEditable() {
        return true;
    }

    @Override
    public void setContent(byte[] newContent) {
        try {
            setNewContents(new String(newContent, "UTF-8"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public ITypedElement replace(ITypedElement dest, ITypedElement src) {
        return null;
    }

    /**
     * Get the new contents.
     *
     * @return the new contents.
     */
    public static String getNewContents() {
        return newContents;
    }

    /**
     * Set the new contents.
     *
     * @param newContents the newContents to set
     */
    public static void setNewContents(String newContents) {
        CompareItem.newContents = newContents;
    }
}
