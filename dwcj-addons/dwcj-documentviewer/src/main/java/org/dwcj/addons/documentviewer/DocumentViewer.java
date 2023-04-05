package org.dwcj.addons.documentviewer;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.component.AbstractComponent;
import org.dwcj.component.HasStyle;
import org.dwcj.component.htmlcontainer.HtmlContainer;
import org.dwcj.component.window.AbstractWindow;


/**
 * The DocViewer widget allows you to put a htmlView onto your application and directly open files
 * in it. For instance, to open a pdf file in your application you can do the following:
 *
 * <pre>
 * {@code
 *   yDocViewer docviewer = new DocViewer();
 *   panel.add(docviewer);
 *   docviewer.open(path-to-pdf-file)
 * }
 * </pre>
 *
 * @author Eric Handtke
 */
public final class DocumentViewer extends AbstractComponent implements HasStyle {
  private HtmlContainer htmlView;

  /**
   * {@inheritDoc}
   */
  @Override
  public DocumentViewer setStyle(String property, String value) {
    htmlView.setStyle(property, value);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DocumentViewer removeStyle(String property) {
    htmlView.removeStyle(property);
    return this;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getComputedStyle(String property) {
    return htmlView.getComputedStyle(property);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getStyle(String property) {
    return htmlView.getStyle(property);
  }

  /**
   * open a document directly from a physical file on disk. The appropriate viewer will be
   * determined as of the extension of the name.
   *
   * @param filePathString The path to the sourcefile that should be opened.
   *
   * @return An instance of this object is returned
   */
  public DocumentViewer open(String filePathString) throws IOException {

    String extension = getExtension(filePathString);

    Path filePath = Paths.get(filePathString.substring(filePathString.lastIndexOf("/") + 1));

    if (extension.equals("pdf")) {

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
   * Open a document directly from a blob in memory. The appropriate viewer will be determined as of
   * the extension of the name.
   *
   * @param blob The document as binary string
   * @param name The file name under which the file should be served
   *
   * @return An instance of this object is returned
   */
  public DocumentViewer open(String blob, String name) throws IOException {
    String tempDir = System.getProperty("java.io.tmpdir");
    String path = tempDir + FileSystems.getDefault().getSeparator() + name;
    File file = new File(path);

    file.deleteOnExit();
    FileOutputStream outputStream = new FileOutputStream(file);
    byte[] strToBytes = blob.getBytes();
    outputStream.write(strToBytes);
    outputStream.close();

    return open(path);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void create(AbstractWindow panel) {

    htmlView = new HtmlContainer();

    panel.add(htmlView);
    htmlView.setStyle("width", "100%");
    htmlView.setStyle("height", "100%");
  }

  private String getExtension(String filePath) {
    String extension = "";
    if (filePath.contains(".")) {
      extension = filePath.substring(filePath.lastIndexOf(".") + 1);
    }
    return extension;
  }
}
