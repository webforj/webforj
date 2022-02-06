package dwcjsample;

import org.dwcj.App;
import org.dwcj.annotations.AppIndex;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.exceptions.DwcAppInitializeException;

@AppIndex(name="Sample App")
public class SampleApp extends App{

	@Override
	public void run() throws DwcAppInitializeException {
		
		new SampleAppPanel();
	}

	private void onButtonPush(ButtonPushEvent buttonpushevent1) {
		App.msgbox("Hello");
	}

}
