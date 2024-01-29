import ast
import chromedriver_autoinstaller
import os

from enum import Enum
from intensity.config import ApplicationVariables
from intensity.logger import get_logger
from selenium import webdriver
from selenium.webdriver.remote.webdriver import WebDriver
from urllib.error import URLError

from intensity.utils.utils import StringUtils

logger = get_logger(__name__)

BROWSER_RESOLUTION = {
    'small': (400, 600),
    'medium': (768, 1024),
    'large': (1080, 1920)
}


class AvailableDrivers(Enum):
    def __str__(self):
        return self.value

    @property
    def value(self):
        return self._name_.lower()

    CHROME, FIREFOX, GRID = list(range(3))


class WebDriverUtils(object):

    @classmethod
    def create_driver(cls, driver_name, timeout, browser_size, user_agent, **params):
        """
        Creates a local Selenium WebDriver instance.
        :param driver_name:  String     Driver name.
        :param timeout:      Int        Timeout in seconds.
        :param browser_size: String or tuple of integers representing the browser size.
                             Ex: 'small', 'medium', 'large', 'max' (for maximized) or (1920, 1080)
        :param user_agent:   String     Firefox user agent.
        """
        drivers = {
            'firefox': (cls._create_firefox_driver, {'user_agent': user_agent}),
            'chrome': (cls._create_chrome_driver, {'size': browser_size}),
            'grid': (cls._create_grid_driver, {'params': params})
        }

        try:
            create_driver, driver_params = drivers[driver_name.lower()]
        except KeyError:
            raise ValueError(
                'The chosen driver name for this test run is not a valid choice: "{}".\n'
                'The following options are currently available: {}.'.format(
                    driver_name, ', '.join([item.value for item in AvailableDrivers])
                )
            )

        driver = create_driver(**driver_params)
        driver.implicitly_wait(timeout)
        cls._resize_browser(driver, browser_size)

        return driver

    @classmethod
    def is_headless(cls):
        return StringUtils.convert_str_to_bool(
            ApplicationVariables().get_section_parameter('selenium', 'is_headless')
        )

    @classmethod
    def _resize_browser(cls, driver, size):
        """
        Resize the browser window
        :param driver:  The current WebDriver instance
        :param size:
            - A string representing a fixed value: "small", "medium" or "large"
            - A tuple of integers representing the resolution. Ex: "size=(1920, 1080)"
        """
        if size == 'max':
            driver.maximize_window()
            return

        error = False
        err_msg = 'Browser size value must be: "small", "medium", "large", "max" ' \
                  'or a tuple of integers representing the resolution.'

        try:
            size = ast.literal_eval(size)
        except ValueError:
            pass

        if isinstance(size, tuple):
            if len(size) != 2 or not isinstance(size[0], int) or not isinstance(size[1], int):
                error = True
        elif isinstance(size, str):
            try:
                size = BROWSER_RESOLUTION[size]
            except KeyError:
                error = True
        else:
            error = True

        if error:
            driver.quit()
            raise ValueError(err_msg)

        driver.set_window_size(width=size[0], height=size[1])

    @classmethod
    def _get_chrome_options(cls, size, mobile_emulation=None):
        options = webdriver.ChromeOptions()

        if cls.is_headless():
            options.headless = True
            if size == 'max':
                options.add_argument('--window-size=1920, 1080')

        options.add_argument('disable-infobars')
        options.add_argument('--disable-popup-blocking')
        options.add_experimental_option('prefs', {
            'credentials_enable_service': False,
            'profile': {
                'password_manager_enabled': False
            },
            "profile.default_content_setting_values.cookies": 1,
            "profile.cookie_controls_mode": 1
        })

        if mobile_emulation:
            options.add_experimental_option('mobileEmulation', mobile_emulation)

        return options

    @classmethod
    def _get_firefox_options(cls, user_agent):
        options = webdriver.FirefoxOptions()

        if cls.is_headless():
            options.headless = True
        if user_agent:
            options.set_preference("general.useragent.override", user_agent)

        options.set_preference("intl.accept_languages", "en-us")
        options.set_preference("browser.link.open_newwindow", 3)
        options.set_preference(
            "network.cookie.cookieBehavior",
            0 if os.environ.get('DISABLE_COOKIES') is None else int(
                os.environ.get('DISABLE_COOKIES')
            )
        )

        return options

    @classmethod
    def _create_firefox_driver(cls, user_agent):
        return webdriver.Firefox(firefox_options=cls._get_firefox_options(user_agent))

    @classmethod
    def _create_chrome_driver(cls, size):
        chromedriver_autoinstaller.install()
        return webdriver.Chrome(chrome_options=cls._get_chrome_options(size))

    @classmethod
    def _create_grid_driver(cls, params):
        """
        Creates a Selenium GRID remote WebDriver instance
        """
        caps = cls._create_remote_webdriver_capabilities(
            params['browser']['name'], params['browser']['version'], params['platform']
        )
        hub_url = params['grid_hub']

        try:
            driver = WebDriver(command_executor='{}/wd/hub'.format(hub_url),
                               desired_capabilities=caps)
        except URLError as err:
            raise URLError(
                'Could not create GRID driver [Hub URL: {}] | {}'.format(hub_url, err.reason))

        return driver

    @classmethod
    def _create_remote_webdriver_capabilities(cls, browser_name, browser_version, platform):
        """
        Creates the required remote WebDriver capabilities
        :param browser_name:    String      Browser's name.
        :param browser_version: String      Browser's version.
        :param platform:        String      The remote platform's name.
        :return:                Dict        Remote WebDriver capabilities.
        """
        browser_name = browser_name.lower()

        if browser_name == 'firefox':
            caps = webdriver.DesiredCapabilities.FIREFOX.copy()
        elif browser_name == 'chrome':
            caps = webdriver.DesiredCapabilities.CHROME.copy()
            caps['chromeOptions'] = cls._get_chrome_options().to_capabilities()[
                'goog:chromeOptions'
            ]
        elif browser_name == 'ie':
            caps = webdriver.DesiredCapabilities.INTERNETEXPLORER.copy()
        elif browser_name == 'safari':
            caps = webdriver.DesiredCapabilities.SAFARI.copy()
        else:
            err_msg = "Could not create {} driver".format(browser_name)
            logger.error(err_msg)
            raise NotImplementedError(err_msg)

        caps['platform'] = platform
        caps['version'] = browser_version

        return caps