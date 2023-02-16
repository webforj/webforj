package org.dwcj.templates;

import java.util.HashMap;

import org.dwcj.App;
import org.dwcj.controls.applayout.AppLayout;
import org.dwcj.controls.label.Label;
import org.dwcj.controls.panels.Div;
import org.dwcj.controls.tabcontrol.TabControl;
import org.dwcj.templates.pages.Counter;
import org.dwcj.templates.pages.Dashboard;
import org.dwcj.templates.pages.TestDivOne;
import org.dwcj.templates.pages.TestDivTwo;
import org.dwcj.util.Assets;

public class AppTemplateTest extends AppLayout{


    HashMap<String, Div> test = new HashMap<>();
             

    public AppTemplateTest(){
        App.addInlineStyleSheet(Assets.contentOf("css/styles.css"));
        
        test.put("Dashboard", new Div().add(new Label("1")));
        test.put("Counter", new Div().add(new Label("2")));
        test.put("Div", new Div().add(new Label("3")));
        test.put("Div2",new Div().add(new Label("4")));
        test.put("Empty Div", new Div().add(new Label("5")));
        /*Creates the App Layout's header */
         Div header = this.getHeader();
         header.addClassName("bbj-toolbar");
         header.add(new Label("<html><bbj-icon-button name='menu-2' data-drawer-toggle></bbj-icon-button></html>"));




         /*Creates the App Layout's drawer */
         Div drawer = this.getDrawer();



         /*Creates a tab control and adds the menu items to the drawer */
         TabControl drawerMenu = new TabControl();
         drawer.add(drawerMenu);
         drawerMenu.setAttribute("nobody","true");
         drawerMenu.setAttribute("borderless","true");
         drawerMenu.setAttribute("placement","left");
         drawerMenu.addTab("Dashboard", new Dashboard());
         drawerMenu.addTab("Counter", new Counter());
         drawerMenu.addTab("Div", new TestDivOne());
         drawerMenu.addTab("Div2", new TestDivTwo());
         
         drawerMenu.onTabSelect((ev) -> {             

            /*== When the divs are created within the class and set to the content section */
            this.setContent(test.get(drawerMenu.getTabAt(ev.getIndex()).getKey()));

            /*== When the content is set from the TabControl divs which are separate classes extending Div
              == I've included a console log to show that the indicies are being correctly selected, when Eric and I tested yesterday,
              == we also tested more extensively to make sure that the indices/Divs in the tab control were correct 
            */

            // App.consoleLog(((Integer) ev.getIndex()).toString());
            // this.setContent(drawerMenu.getTabAt(ev.getIndex()).getValue());


            /*== When the content is a new Div each time (only one that works, not ideal use case)*/
            // this.setContent(new Div().add(new Label(((Integer)ev.getIndex()).toString())));
            
         });
    }

}
