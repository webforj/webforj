package org.dwcj.test.controls;

import org.dwcj.App;
import org.dwcj.controls.Button;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;

public class ButtonTestApp extends App {

    private AppPanel appPanel;


    private boolean buttonClicked = false;

    public Button getTestButton() {
        return testButton;
    }

    public boolean isButtonClicked() {
        return buttonClicked;
    }

    private Button testButton;

    @Override
    public void run() throws DwcAppInitializeException {
        appPanel = new AppPanel();
        testButton = new Button();
        testButton.setText("test");
        appPanel.add(testButton);    }

    public void registerButtonClickEvent(){
        testButton.onClick(this::onTestButtonClick);
    }

    private void onTestButtonClick(ButtonPushEvent buttonPushEvent) {
        this.buttonClicked = true;
    }

    public void performButtonClick() {
        this.testButton.performClick();
    }
}
