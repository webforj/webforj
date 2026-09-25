package com.devtoolsapplayoutspring.views;

import com.webforj.component.Composite;
import com.webforj.component.Theme;
import com.webforj.component.button.Button;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.component.toast.Toast;

/** Compiled application view whose nested class resolves to this source file. */
public class ResolvedButtonView extends Composite<FlexLayout> {

  /** Creates the view with its nested button group. */
  public ResolvedButtonView() {
    getBoundComponent().add(new ButtonGroup());
  }

  static class ButtonGroup extends Composite<FlexLayout> {
    private final Button action = new Button("Resolved action");

    ButtonGroup() {
      action.setTooltipText("Resolved tooltip");
      getBoundComponent().add(action);
      action.onClick(event -> Toast.show("Resolved action clicked", 3000, Theme.INFO,
          Toast.Placement.BOTTOM_RIGHT));
    }
  }
}
