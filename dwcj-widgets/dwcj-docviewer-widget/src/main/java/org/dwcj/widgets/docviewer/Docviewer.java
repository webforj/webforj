package org.dwcj.widgets.docviewer;

import org.dwcj.controls.htmlcontainer.HtmlContainer;
import org.dwcj.controls.panels.AbstractPanel;
import org.dwcj.controls.panels.Div;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.controls.AbstractControl;

public class Docviewer extends AbstractControl {

    private HtmlContainer htmlView;

    @Override
    protected void create(AbstractPanel panel) {
        super.create(panel);

        htmlView = new HtmlContainer();

        panel.add(htmlView);
        htmlView.setStyle("width", "100%");
        htmlView.setStyle("height", "100%");
    }

    public void setStyle(String property, String value) {
        htmlView.setStyle(property, value);
    }

     /**
      * open: open a document directly from a phyiscal file on disk
      * 
      * @param String filePathString : the path to the sourcefile that should be opened
      * 
      * The approrpiate viewer will be determined as of the extention of the name
      */
    public void open(String filePathString) {

        String extension = getExtension(filePathString);

        // filePathString = filePathString.replaceAll("\\", "/");
        App.consoleLog(extension);
        Path filePath = Paths.get(filePathString.substring(filePathString.lastIndexOf("/") + 1));
        switch (extension) {
            case "pdf":
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
                break;
            case "htm":
                try {
                    filePath = Paths.get(filePathString);
                    String content = Files.readString(filePath);
                    htmlView.setText(content);
                } catch (IOException ex) {
                    App.consoleLog("Error in case htm: " + ex.toString());
                }
                break;
            case "html":
                try {
                    filePath = Paths.get(filePathString);
                    String content = Files.readString(filePath);
                    htmlView.setText(content);
                } catch (IOException ex) {
                    App.consoleLog("Error in case htm: " + ex.toString());
                }
                break;
            default:
                try {
                    filePath = Paths.get(filePathString);
                    String content = Files.readString(filePath);
                    htmlView.setText(content);
                } catch (IOException ex) {
                    App.consoleLog(ex.getMessage());
                }
                break;
        }

    }

    /**
     * openBlob: open a document directly from a blob in memory
     * 
     * @param String blob : the document as binary string
     * @param String name : the file name under which the file should be served
     * 
     * The approrpiate viewer will be determined as of the extention of the name
     *                  
     */
    public void openBlob(String blob, String name) {

        try {
            Path tempD = Files.createTempDirectory("__fviewer");
            App.consoleLog("test");
            String tempDString = tempD.toAbsolutePath().toString() + "/";
            String path = tempDString + name;
            File file = new File(path);

            file.deleteOnExit();
            FileOutputStream outputStream = new FileOutputStream(file);
            byte[] strToBytes = blob.getBytes();
            outputStream.write(strToBytes);
            outputStream.close();

            open(path);
        } catch (IOException ex) {
            App.consoleLog(ex.toString());
        }

    }

    public String getExtension(String filePath) {
        String extension = "";

        extension = filePath.substring(filePath.lastIndexOf(".") + 1);

        return extension;
    }
}
