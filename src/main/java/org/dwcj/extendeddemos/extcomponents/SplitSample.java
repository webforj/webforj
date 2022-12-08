// package org.dwcj.extendeddemos.extcomponents;

// import org.dwcj.App;
// import org.dwcj.controls.button.Button;
// import org.dwcj.controls.label.Label;
// import org.dwcj.exceptions.DwcAppInitializeException;
// import org.dwcj.controls.panels.AppPanel;
// import org.dwcj.shoelacecontrols.SplitPanel;

// public class SplitSample extends App {

//     @Override
//     public void run() throws DwcAppInitializeException {

//         AppPanel p = new AppPanel();

//         Label headline = new Label("<html><h2>Split Panel Demo</h2>");
//         p.add(headline);

//         SplitPanel sp = new SplitPanel(false);
//         sp.setPosition(20);
//         p.add(sp);

//         sp.setStyle("height", "400px");
//         sp.setStyle("height", "500px");
//         sp.setStyle("width", "100%");
//         sp.getStartPanel().setStyle("background-color", "red");

//         sp.getStartPanel().add(new Button("getStartPanel"));
//         sp.getStartPanel().add(new Button("getStartPanel1"));

//         sp.getEndPanel().setStyle("background-color", "blue");


//         sp.getEndPanel().add(new Button("TEST"));
//         sp.getEndPanel().add(new Button("TEST1"));


//     }


// }
