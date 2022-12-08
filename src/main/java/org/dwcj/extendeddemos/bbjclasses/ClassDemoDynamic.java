package org.dwcj.extendeddemos.bbjclasses;

import org.dwcj.App;
import org.dwcj.Environment;
import org.dwcj.bridge.IDwcjBBjBridge;
import org.dwcj.controls.button.Button;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.textbox.TextBox;
import org.dwcj.controls.button.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.controls.panels.AppPanel;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;

public class ClassDemoDynamic extends App {

    private TextBox ipa;
    private TextBox ipb;
    private String programsource;
    private Label result;

    @Override
    public void run() throws DwcAppInitializeException {

        AppPanel panel = new AppPanel();
        panel.setStyle("padding","20px");
        panel.setStyle("display","inline-grid");
        panel.setStyle("gap","10px");

        Label headline = new Label("<html><h1>Using BBj Classes Dynamically</h1>");
        panel.add(headline);

        Label explanation = new Label("<html><p>This Demo Shows how to use the following BBj class:</p>");
        panel.add(explanation);

        programsource = "use java.math.BigDecimal\n" +
                        "CLASS PUBLIC MyClass\n\n" +
                        "  METHOD PUBLIC BigDecimal multiply(BigDecimal factor1!, BigDecimal factor2!)\n" +
                        "    METHODRET BigDecimal.valueOf(factor1! * factor2!)\n" +
                        "  METHODEND\n\n" +
                        "CLASSEND";

        //this could later be changed to a MultiLineEdit so the user of this demo can edit the call
        Label sourcecode = new Label(programsource);
        sourcecode.setStyle("font-family","monospace");
        panel.add(sourcecode);

        ipa = new TextBox("4.125");
        ipa.setAttribute("type","number");
        ipa.setAttribute("step","0.001");
        ipa.setAttribute("label","Value for A (Numeric)");
        ipa.setStyle("padding-top","40px");
        panel.add(ipa);

        ipb = new TextBox("5");
        ipb.setAttribute("type","number");
        ipb.setAttribute("step","0.001");
        ipb.setAttribute("label","Value for B (Numeric)");
        ipb.setStyle("padding-top","20px");
        panel.add(ipb);

        Button exec = new Button("Make CALL");
        panel.add(exec);
        exec.onClick(this::onExecClick);
        
        result = new Label("");
        result.setStyle("font-family","monospace");
        panel.add(result);

    }

    private void onExecClick(ButtonPushEvent buttonPushEvent) {

        File f = null;
        try {
            f = File.createTempFile("pgm_",".bbj");
            f.deleteOnExit();
            BufferedWriter writer = new BufferedWriter(new FileWriter(f.getAbsolutePath()));
            writer.write(this.programsource);
            writer.close();
        } catch (IOException e) {
            msgbox(e.getMessage(),0,"Could not create temporary program file on disk.");
            throw new RuntimeException(e);
        }

        IDwcjBBjBridge helper = Environment.getInstance().getDwcjHelper();

        String classname = "::"+f.getAbsolutePath()+"::MyClass";
        Object myObjectInstance = helper.createInstance(classname);

        ArrayList<BigDecimal> args = new ArrayList<>();
        args.add(new BigDecimal(ipa.getText()));
        args.add(new BigDecimal(ipb.getText()));


        BigDecimal retval  = (BigDecimal) helper.invokeMethod(myObjectInstance,"multiply", args);

        String r = "Result: "+retval+"\n";
        result.setText(r);


    }
}
