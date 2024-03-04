package com.sample;

import com.webforj.App;
import com.webforj.component.list.ChoiceBox;
import com.webforj.component.navigator.Navigator;
import com.webforj.component.navigator.Navigator.Layout;
import com.webforj.component.navigator.Navigator.Part;
import com.webforj.component.tabbedpane.TabbedPane;
import com.webforj.component.window.Frame;
import com.webforj.exceptions.DwcjException;

public class NF extends App {
  TabbedPane pane = new TabbedPane();
  ChoiceBox placements = new ChoiceBox("Placement");

  @Override
  public void run() throws DwcjException {

    Navigator nav = new Navigator(100, Layout.PAGES);
    // nav.setAttribute("layout", "none");

    nav.setText("First", Part.FIRST_BUTTON);
    nav.setTooltipText("First", Part.FIRST_BUTTON);

    nav.setText("Previous", Part.PREVIOUS_BUTTON);
    nav.setTooltipText("Previous", Part.PREVIOUS_BUTTON);

    nav.setText("Next", Part.NEXT_BUTTON);
    nav.setTooltipText("Next", Part.NEXT_BUTTON);

    nav.setText("Last", Part.LAST_BUTTON);
    nav.setTooltipText("Last", Part.LAST_BUTTON);

    nav.setText("'0' + page", Part.PAGE_BUTTON);
    nav.setTooltipText("Navigate To", Part.PAGE_BUTTON);

    nav.onChange(e -> {
      consoleLog("==================== Event");
      consoleLog("Direction: " + e.getDirection());
      consoleLog("Current: " + e.getCurrent());
      consoleLog("Start Index: " + e.getStartIndex());
      consoleLog("End Index: " + e.getEndIndex());
      consoleLog("==================== Paginator");
      consoleLog("Current: " + e.getComponent().getPaginator().getCurrent());
      consoleLog("Start Index: " + e.getComponent().getPaginator().getStartIndex());
      consoleLog("End Index: " + e.getComponent().getPaginator().getEndIndex());
    });

    Frame window = new Frame();
    window.add(nav);
  }
}

