package dwcjsample;

import org.dwcj.App;
import org.dwcj.exceptions.DwcAppInitializeException;

public class SampleApp extends App {

    @Override
    public void run() throws DwcAppInitializeException {

        new SampleAppPanel();

    }

}
