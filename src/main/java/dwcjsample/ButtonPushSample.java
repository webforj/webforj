package dwcjsample;

import org.dwcj.App;
import org.dwcj.controls.Button;
import org.dwcj.controls.Label;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;
import org.dwcj.panels.AppPanel;

public class ButtonPushSample extends App {
    private Button button;
    private Label output;

    @Override
    public void run() throws DwcAppInitializeException {
        AppPanel panel = new AppPanel();
        button = new Button("Push me!");
        panel.add(button);

        output = new Label("not pushed");
        panel.add(output);

        button.onClick(this::onButtonClick);
        
    }

    private void onButtonClick(ButtonPushEvent buttonPushEvent) {
        button.setText("pushed");
        output.setText("pushed");
    }
}
