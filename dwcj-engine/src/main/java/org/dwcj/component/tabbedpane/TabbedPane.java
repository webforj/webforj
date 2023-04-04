package org.dwcj.component.tabbedpane;

import com.basis.bbj.proxies.sysgui.BBjTabCtrl;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.tabbedpane.event.TabSelectEvent;
import org.dwcj.component.tabbedpane.sink.TabSelectEventSink;
import org.dwcj.component.window.AbstractWindow;
import org.dwcj.component.window.Panel;

import java.util.ArrayList;
import java.util.AbstractMap.SimpleEntry;
import java.util.function.Consumer;

public final class TabbedPane extends AbstractDwcComponent {

  /** Event sink for selection of a tab */
  private TabSelectEventSink tabSelectEventSink;
  /** List of all callbacks to be added to the tab */
  private ArrayList<Consumer<TabSelectEvent>> callbacks = new ArrayList<>();
  /** List of tabs associated with the control */
  private ArrayList<SimpleEntry<String, Panel>> tabs = new ArrayList<>();
  /** The tab control's theme */
  private Theme theme = Theme.DEFAULT;
  /** The tab control's expanse */
  private Expanse expanse = null;
  /** The panel which the tab control belongs to */
  private AbstractWindow parentPanel;
  /** The currently selected tab */
  private int selected = 0;

  public enum Expanse {
    LARGE, MEDIUM, SMALL, XLARGE, XSMALL
  }

  public enum Theme {
    DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
  }

  protected BBjTabCtrl tabCtrl;

  @Override
  protected void create(AbstractWindow p) {
    try {
      BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
      parentPanel = p;
      // todo: honor visibility flag, if set before adding the control to the form, so
      // it's created invisibly right away
      tabCtrl = w.addTabCtrl(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1,
          BASISNUMBER_1);
      ctrl = tabCtrl;
      catchUp();
    } catch (Exception e) {
      Environment.logError(e);
    }
  }

  /**
   * Add a tab to the tab control
   *
   * @param text the text to display on the tab control
   * @return
   */
  public TabbedPane add(String text) {

    if (this.ctrl != null) {
      try {
        this.tabCtrl.addTab(text, -1);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.add(new SimpleEntry<>(text, null));
    }

    return this;
  }

  /**
   * Add a tab and add a Div to it. Important: The DIV has to exist on the parent panel, you need to
   * call "add" before passing it to the tab control.
   *
   * @param text The text for the tab
   * @param panel the panel to attach to the tab
   * @return the Tab Control object
   */
  public TabbedPane add(String text, Panel panel) {
    if (this.ctrl != null) {
      try {
        parentPanel.add(panel);
        this.tabCtrl.addTab(text, WindowAccessor.getDefault().getBBjWindow(panel));
      } catch (BBjException | IllegalAccessException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.add(new SimpleEntry<>(text, panel));
    }

    return this;
  }

  /**
   * Returns the title of the tab at the given index
   *
   * @param index Desired index number
   * @return The title of the tab
   */
  public String getTitleAt(int index) {
    if (this.ctrl != null) {
      try {
        return this.tabCtrl.getTitleAt(index);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabs.get(index).getKey();
  }

  /**
   * Gets the number of tabs in the tab control
   *
   * @return The number of tabs
   */
  public int getTabCount() {
    if (this.ctrl != null) {
      try {
        return this.tabCtrl.getNumTabs();
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    return this.tabs.size();
  }

  /**
   * Inserts a tab without an associated Div at a specific index
   *
   * @param index Desired index for the new tab
   * @param title Title for the new tab
   * @return The control itself
   */
  public TabbedPane insert(int index, String text) {
    if (this.ctrl != null) {
      try {
        this.tabCtrl.insertTab(index, text, -1);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.add(index, new SimpleEntry<>(text, null));
    }
    return this;
  }

  /**
   * Inserts a tab with an associated Div at a specific index
   *
   * @param index Desired index for the new tab
   * @param title Title for the new tab
   * @param panel Div to be associated with the new tab
   * @return The control itself
   */
  public TabbedPane insert(int index, String text, Panel panel) {
    if (this.ctrl != null) {
      try {
        parentPanel.add(panel);
        this.tabCtrl.insertTab(index, text, WindowAccessor.getDefault().getBBjWindow(panel));
      } catch (BBjException | IllegalAccessException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.add(index, new SimpleEntry<>(text, panel));
    }
    return this;
  }

  /**
   * Removes a tab from the tab control based on its index
   *
   * @param index Index of the tab designated for removal
   * @return The control itself
   */
  public TabbedPane remove(int index) {
    if (this.ctrl != null) {
      try {
        this.tabCtrl.removeTab(index);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.remove(index);
    }
    return this;
  }

  /**
   * Put a DIV under an existing tab Important: The DIV has to exist on the parent panel, you need
   * to call "add" before passing it to the tab control.
   *
   * @param index the zero-based index of the tab
   * @param panel the DIV panel to put under the tab
   * @return the Tab Control object itself
   */
  public TabbedPane setPanelAt(int index, Panel panel) {
    if (this.ctrl != null) {
      try {
        parentPanel.add(panel);
        this.tabCtrl.setControlAt(index, WindowAccessor.getDefault().getBBjWindow(panel));
      } catch (BBjException | IllegalAccessException e) {
        Environment.logError(e);
      }
    } else {
      this.tabs.get(index).setValue(panel);
    }
    return this;
  }

  /**
   * Designates which of the tabs should be selected
   *
   * @param index Index of tab designated for selection
   * @return The control itself
   */
  public TabbedPane selectIndex(int index) {
    if (this.ctrl != null) {
      try {
        this.tabCtrl.setSelectedIndex(index);
      } catch (BBjException e) {
        Environment.logError(e);
      }
    }
    this.selected = index;
    return this;
  }

  /**
   * register an event callback for the click event
   *
   * @param callback A method to receive the click event
   * @return the control itself
   */

  public TabbedPane onSelect(Consumer<TabSelectEvent> callback) {
    if (this.ctrl != null) {
      if (this.tabSelectEventSink == null) {
        this.tabSelectEventSink = new TabSelectEventSink(this);
      }
      this.tabSelectEventSink.addCallback(callback);
    } else {
      this.callbacks.add(callback);
    }
    return this;
  }

  @Override
  public TabbedPane setText(String text) {
    super.setText(text);
    return this;
  }

  @Override
  public TabbedPane setVisible(Boolean visible) {
    super.setVisible(visible);
    return this;
  }

  @Override
  public TabbedPane setEnabled(Boolean enabled) {
    super.setEnabled(enabled);
    return this;
  }

  @Override
  public TabbedPane setTooltipText(String text) {
    super.setTooltipText(text);
    return this;
  }

  @Override
  public TabbedPane setAttribute(String attribute, String value) {
    super.setAttribute(attribute, value);
    return this;
  }

  @Override
  public TabbedPane setId(String elementId) {
    super.setId(elementId);
    return this;
  }

  @Override
  public TabbedPane setStyle(String property, String value) {
    super.setStyle(property, value);
    return this;
  }

  @Override
  public TabbedPane addClassName(String selector) {
    super.addClassName(selector);
    return this;
  }

  @Override
  public TabbedPane removeClassName(String selector) {
    super.removeClassName(selector);
    return this;
  }

  public TabbedPane setExpanse(Expanse expanse) {
    super.setControlExpanse(expanse);
    return this;
  }

  public TabbedPane setTheme(Theme theme) {
    super.setControlTheme(theme);
    return this;
  }

  @Override
  @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list
                                  // of checks
  protected void catchUp() throws IllegalAccessException {
    if (Boolean.TRUE.equals(this.getCaughtUp()))
      throw new IllegalAccessException("catchUp cannot be called twice");

    super.catchUp();

    if (!this.callbacks.isEmpty()) {
      this.tabSelectEventSink = new TabSelectEventSink(this);
      while (!this.callbacks.isEmpty()) {
        this.tabSelectEventSink.addCallback(this.callbacks.remove(0));
      }
    }

    /*
     * Reimplemented logic instead of calling method to avoid duplicate tabs being added
     */
    tabs.forEach(n -> {
      if (n.getValue() == null) {
        this.add(n.getKey());
      } else {
        this.add(n.getKey(), n.getValue());
      }
    });

    if (this.expanse != null) {
      this.setExpanse(this.expanse);
    }

    if (this.theme != Theme.DEFAULT) {
      this.setTheme(this.theme);
    }

    if (this.selected != 0) {
      this.selectIndex(this.selected);
    }

  }

}
