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

public class AppTemplate extends AppLayout{

    Label title = new Label("DWC Application");
    String img = "https://i.ibb.co/1n4n1Nh/logo.png";
    HashMap<String, Div> test = new HashMap<>();
             

    public AppTemplate(){
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
         header.add(this.title);
         this.title.addClassName("headerTitle");



         /*Creates the App Layout's drawer */
         Div drawer = this.getDrawer();

         /*Creating and adding/styling the logo */
         Div logoDiv = new Div().addClassName("logo");
         Label logo = new Label("<html><img src='" + this.img + "'</img></html>");
         drawer.add(logoDiv);
         logoDiv.add(logo);



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
         drawerMenu.addTab("Empty Div", new Div());
        //  drawerMenu.addTab("<bbj-icon name='users'></bbj-icon>          Customers");
        //  drawerMenu.addTab("<bbj-icon name='box'></bbj-icon>            Products");
        //  drawerMenu.addTab("<bbj-icon name='files'></bbj-icon>          Documents");
        //  drawerMenu.addTab("<bbj-icon name='checklist'></bbj-icon>      Tasks");
        //  drawerMenu.addTab("<bbj-icon name='chart-dots-2'></bbj-icon>   Analytics");

         /*Sample content on the page */
         //  content.add(new Dashboard());
         //  content.add(new Label("<html><h1>Application Title</h1></html>"));
         
         //  Label pageContent = new Label("<html><p>Content goes here</p></html>");
         //  content.add(pageContent);
         
         drawerMenu.onTabSelect((ev) -> {
             //  pageContent.setText("Content for " + ev.getTitle().replaceAll("<[^>]*>","").trim() + " goes here");
             // this.setContent(new Dashboard());
             Integer index = ev.getIndex();
             
             App.consoleLog(index.toString());
             // Label test = new Label("Content for " + drawerMenu.getTabAt(ev.getIndex()).getKey().replaceAll("<[^>]*>","").trim() + " goes here");
            //  this.setContent(drawerMenu.getTabAt(ev.getIndex()).getValue());
             //App.consoleLog(drawerMenu.getTabAt(ev.getIndex()).toString());
             //Div content = drawerMenu.getTabAt(ev.getIndex()).getValue();
             
      
            App.consoleLog(drawerMenu.getTabAt(ev.getIndex()).getKey());
            // content.add(test);
            this.setContent(test.get(drawerMenu.getTabAt(ev.getIndex()).getKey()));
            // this.setContent(new Div().add(new Label(index.toString())));
         });
    }

}
