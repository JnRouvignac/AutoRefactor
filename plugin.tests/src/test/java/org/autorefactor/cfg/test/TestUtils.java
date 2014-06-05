package org.autorefactor.cfg.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

public class TestUtils {

	public static String readAll(File file) throws IOException {
		// FIXME Java 7 version of this method:
		// return new String(Files.readAllBytes(file.toPath()), "UTF8");
		FileInputStream fis = null;
		try {
			fis = new FileInputStream(file);
			final InputStreamReader reader = new InputStreamReader(fis);
			final StringBuilder sb = new StringBuilder();
			final char[] buf = new char[4096];
			int nbRead;
			while ((nbRead = reader.read(buf)) != -1) {
				sb.append(buf, 0, nbRead);
			}
			return sb.toString();
		} finally {
			if (fis != null) {
				fis.close();
			}
		}
	}

}
