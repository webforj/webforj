package org.dwcj.widgets.docviewer;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.panels.AbstractPanel;
import org.dwcj.interfaces.HasStyle;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.controls.AbstractControl;

/**
 * The DocViewer widget allows you to put a htmlView onto your application and diretly
 * open files in it.
 * 
 * For instance, to open a pdf file in your application you can do the
 * following:
 * 
 * <pre>
 * {@code
 * 	DocViewer docviewer = new DocViewer();
 * 	panel.add(docviewer);
 * 	docviewer.open(path-to-pdf-file)
 * }
 * </pre>
 * 
 * @author Eric Handtke
 */

public final class DocViewer extends AbstractControl implements HasStyle {

	
	private HtmlContainer htmlView;

	@Override
	protected void create(AbstractPanel panel) {
		super.create(panel);

		htmlView = new HtmlContainer();

		panel.add(htmlView);
		htmlView.setStyle("width", "100%");
		htmlView.setStyle("height", "100%");
	}

	public DocViewer setStyle(String property, String value) {
		htmlView.setStyle(property, value);
		return this;
	}

	/**
	 * open a document directly from a phyiscal file on disk.
	 * The approrpiate viewer will be determined as of the extention
	 * of the name.
	 * 
	 * @param filePathString The path to the sourcefile that should be
	 * opened.
	 * 
	 * @return An instance of this object is returned          
	 */
	public DocViewer open(String filePathString) throws IOException {

		String extension = getExtension(filePathString);

		Path filePath = Paths.get(filePathString.substring(filePathString.lastIndexOf("/") + 1));
		if (extension == "pdf") {

			IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();
			Object instance = helper.createInstance("::BBUtils.bbj::BBUtils");

			ArrayList<Object> args = new ArrayList<>();
			args.add(filePathString); // p_srcFile$
			args.add(filePath.toString()); // p_destDir$
			args.add(""); // p_destFile$
			args.add(0); // p_requireSSL
			args.add(0); // p_obfuscate

			String result = (String) helper.invokeMethod(instance, "copyFileToWebServer", args);
			htmlView.setUrl(result);

		} else {

			filePath = Paths.get(filePathString);
			String content = Files.readString(filePath);
			htmlView.setText(content);

		}
		return this;
	}

	/**
	 * Open a document directly from a blob in memory. The approrpiate viewer will be determined as of the extention
	 * of the name.
	 * 
	 * @param blob  The document as binary string
	 * @param name  The file name under which the file should be served
	 * 
	 * @return An instance of this object is returned 
	 */
	public DocViewer open(String blob, String name) throws IOException {

		Path tempD = Files.createTempDirectory("__fviewer");

		String tempDString = tempD.toAbsolutePath().toString() + "/";
		String path = tempDString + name;
		File file = new File(path);

		file.deleteOnExit();
		FileOutputStream outputStream = new FileOutputStream(file);
		byte[] strToBytes = blob.getBytes();
		outputStream.write(strToBytes);
		outputStream.close();

		return open(path);

	}

	public String getExtension(String filePath) {
		String extension = "";

		extension = filePath.substring(filePath.lastIndexOf(".") + 1);

		return extension;
	}
}
