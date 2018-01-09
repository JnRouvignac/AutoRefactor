package org.autorefactor.ui;

import java.lang.reflect.InvocationTargetException;
import java.io.*;
import org.eclipse.compare.structuremergeviewer.ICompareInput;
import org.eclipse.compare.structuremergeviewer.ICompareInputChangeListener;

import org.autorefactor.refactoring.ApplyRefactoringsJob;
import org.eclipse.compare.*;
import org.eclipse.compare.structuremergeviewer.DiffNode;
import org.eclipse.compare.structuremergeviewer.Differencer;
import org.eclipse.core.runtime.IProgressMonitor;

import org.autorefactor.refactoring.RefactoringUnit;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.internal.corext.util.Resources;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.core.runtime.CoreException;
public class CompareInput extends CompareEditorInput  {
	
	private String original= "Original";
	private String refactored = "Refactored";
	public static final String IGNORE_WHITESPACE = "IGNORE_WHITESPACE";
	static CompareConfiguration compareConfiguration = new CompareConfiguration();
	static DiffNode node;
	static IProgressMonitor monitor;
	File file1 = new File("C:\\Users\\User\\Desktop\\JavaClass1.java");
	File file2 = new File("C:\\Users\\User\\Desktop\\JavaClass2.java");
	CompareItem left;
	CompareItem right;
	public CompareInput() {
		super(compareConfiguration);
		compareConfiguration.setLeftLabel("Original Java File");
		
		compareConfiguration.setRightLabel("Refactored Java File");
		compareConfiguration.setRightEditable(true);
		compareConfiguration.setLeftEditable(true);
		compareConfiguration.setProperty(IGNORE_WHITESPACE, IGNORE_WHITESPACE);
		
		setDirty(true);
		
		
		
		// TODO Auto-generated constructor stub
	}

	
	@SuppressWarnings("restriction")
	@Override
	protected Object prepareInput(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException
	{

		
		CompareItem ancestor = new CompareItem(ApplyRefactoringsJob.codeToRefactor);
		left = new CompareItem(ApplyRefactoringsJob.codeToRefactor);
		
		right = new CompareItem(ApplyRefactoringsJob.refactoredContent);
		
		node = new DiffNode(null, Differencer.CONFLICTING, ancestor, left, right);
		
		return node;
	}

	@Override
	public boolean okPressed() {
	
		
		ITypedElement itypeElement = node.getRight();
		
		
		try {
			
			CompareItem.newContents = ApplyRefactoringsJob.refactoredContent;
			saveChanges(monitor);
			
		}
		catch(Exception e) {
			e.printStackTrace();
		}
		createAndCommit();
		return true;
	}
	
	@Override
	public void cancelPressed() {
	try {
		ApplyRefactoringsJob.newFile.delete(true, monitor);
	} catch (CoreException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
	
	}
	public void createAndCommit() {
		ICompilationUnit iCompilation = ApplyRefactoringsJob.iCompile;
		try {
			
			iCompilation.getBuffer().setContents(CompareItem.newContents);//.toString());
			
			ApplyRefactoringsJob.newFile.delete(true, monitor);
		
		}
		
		catch(Exception e) {
			System.out.println("Exception in create and commit");
			e.printStackTrace();
			
		}
		
	}
	private static String getStringFromInputStream(InputStream is) {

		BufferedReader br = null;
		StringBuilder sb = new StringBuilder();

		String line;
		try {

			br = new BufferedReader(new InputStreamReader(is));
			while ((line = br.readLine()) != null) {
				sb.append(line);
				sb.append("\n");
			}

		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

		return sb.toString();

	}


	public byte[] inputStreamToByte( InputStream is) {
		ByteArrayOutputStream buffer = new ByteArrayOutputStream();

		int nRead;
		byte[] data = new byte[16384];

		try {
			while ((nRead = is.read(data, 0, data.length)) != -1) {
			  buffer.write(data, 0, nRead);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			buffer.flush();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return buffer.toByteArray();
	}


		
	}






