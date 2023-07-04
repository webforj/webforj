# The Dynamic Web Client for Java
<p>

[![Java Tests & Coverage](https://github.com/DwcJava/engine/actions/workflows/maven-test.yml/badge.svg?branch=main)](https://github.com/DwcJava/engine/actions/workflows/maven-test.yml)
[![Publish Snapshot](https://github.com/DwcJava/engine/actions/workflows/maven-publish.yml/badge.svg?branch=main)](https://github.com/DwcJava/engine/actions/workflows/maven-snapshot.yml)
[![reviewdog](https://github.com/DwcJava/engine/actions/workflows/reviewdog.yml/badge.svg)](https://github.com/DwcJava/engine/actions/workflows/reviewdog.yml)

</p>

A robust and flexible framework that can help you deliver a modern and engaging web user interface with ease. In Java.

- **Event Handling**: Handle user interactions and events with ease using the DWCJ's event system. Respond to user actions and update the UI accordingly.

- **Component-Based**: Create reusable and composable components to build complex UIs. Components encapsulate their own state and logic, providing a modular and maintainable structure for your application.

- **Reliable**: Our team in the United States and Europe have been building tools and technologies to help our clients fulfil their needs for over 35 years. We have consistently produced reliable and innovative technologies to ensure our clients have every tool in their kit needed to tackle modern, ever-changing needs.

## Documentation

The DWCJ's documentation site can be found [at this link](https://dwcj.org/) which contains guides, API references, and examples to help you get started with the DWCJ Engine.

The following documentation sections may be useful for those beginning their usage of the DWCJ:

- [DWCJ Installation](https://dwcj.org/docs/installation/local_install)
- [Configuring your application](https://dwcj.org/docs/getting_started/configuration)
- [Creating an application](https://dwcj.org/docs/getting_started/creating_an_application)
- [Component Overviews](https://dwcj.org/docs/components/home)

## Examples

The DWCJ's [HelloWorld repository](https://github.com/DwcJava/HelloWorldJava) contains a sample program which can be run in GitHub codespaces, Docker, or locally and demonstrates the basics for creating your first DWCJ program. Here is the class created in the sample:

```java
@InlineStyleSheet(/* css */"""
    .frame {
      display: inline-grid;
      gap: 20px;
      margin: 20px;
      padding: 20px;
      border: 1px dotted;
    }
      """)
public class HelloWorldJava extends App {

  @Override
  public void run() throws DwcjException {

    Frame frame = new Frame();
    frame.addClassName("frame");

    Label label = new Label("Hello World!");

    Button btn = new Button("Say Hello");
    btn.setTheme(Button.Theme.SUCCESS)
        .setExpanse(Button.Expanse.XLARGE)
        .onClick(e -> msgbox("Hello World!"));

    frame.add(label, btn);
  }
}
```

The above program creates some static text and a button which displays a popup when pushed. 

## Contributing

Contributions to the DWCJ Engine project are welcome! If you would like to contribute, please follow the guidelines outlined in the [CONTRIBUTING.md](https://github.com/DwcJava/engine/blob/main/CONTRIBUTING.md) file.

## License

The DWCJ Engine is licensed under the [MIT License](https://github.com/DwcJava/engine/blob/main/LICENSE).