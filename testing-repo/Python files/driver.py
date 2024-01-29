from intensity.config import ApplicationVariables
from intensity.logger import get_logger
from intensity.utils.utils import poll_data
from selenium.common.exceptions import ElementNotVisibleException, NoSuchElementException, \
    StaleElementReferenceException, WebDriverException, TimeoutException, \
    ElementClickInterceptedException, ElementNotInteractableException
from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait
from urllib.parse import urlparse

logger = get_logger(__name__)


class WebDriverFacade(object):
    """
    Facade that encapsulates all the interactions with web driver
    """
    app_vars = ApplicationVariables()
    global_timeout = app_vars.get_timeout()

    def __init__(self, driver):
        self.driver = driver

    def open(self, url):
        """
        Opens the page at the given url.
        :param url:     String      Url to be opened.
        """
        self.driver.get(url)

    def refresh_webpage(self):
        """
        Refresh the current webpage.
        """
        self.driver.refresh()

    def get_current_url(self):
        """
        Gets the current url.
        Now this is a text to test long documentation:
        a second line to test long documentation
        a third line to test long documentation
        a forth line to test long documentation
        a fifth line to test long documentation
        a sixth line to test long documentation
        a seventh line to test long documentation
        a eighth line to test long documentation
        a nineth line to test long documentation
        a tenth line to test long documentation
        :return:    String      Page url.
        """
        return self.driver.current_url

    def get_current_url_domain(self):
        """
        Gets the current url domain.
        :return:    String      Page domain.
        """
        return urlparse(self.driver.current_url).netloc

    def get_default_timeout(self):
        """
        Gets the current global timeout
        :return     Int     global timeout.
        """
        return int(self.global_timeout)

    def _get_element(self, locator, index=0):
        """
        Finds an element by a locator and an index.
        """
        if index < 0:
            raise ValueError("Index must be a greater than or equals zero")

        elements = self.driver.find_elements(*locator)
        if not elements:
            raise NoSuchElementException(
                "There couldn't be found any elements with the following "
                "selector: %s" % str(locator))
        return elements[index]

    def get_text(self, locator, index=0):
        """
        Returns the text of the element at the given index from locator.
        """
        element = self._get_element(locator, index)

        if element.tag_name == 'img':
            element_text = element.get_attribute('alt')
        else:
            element_text = element.text

        return element_text

    def clear(self, locator):
        """
        Clears the field in the given locator.
        For Chrome, sends Ctrl + A and Backspace to delete all characters from the input,
        where \ue009 is Left Control key and \ue003 is Backspace key.
        """
        element = self.driver.find_element(*locator)

        if self.driver.name == 'chrome':
            element.send_keys('\ue009' + 'a')
            element.send_keys('\ue003')
        else:
            element.clear()

    def send_keys(self, locator, keys):
        """
        Sends keys to the given locator
        """
        self.driver.find_element(*locator).send_keys(keys)

    def clear_and_send_keys(self, locator, keys):
        """
        Clear the given locator and then Sends keys
        """
        self.clear(locator)
        self.send_keys(locator, keys)

    def wait_and_click(self, locator, timeout=global_timeout, index=0):
        """
        Wait timeout seconds for visibility of element
        and click on the element at the given locator
        :param locator:     Locator of element to wait and click.
        :param timeout:     The timeout to wait the element to be visible.
                            By default is global_timeout.
        :param index:       If locator returns only one element, index should be 0.
                            If locator returns more than one element, index should be > 0.
        """
        try:
            self.find_element(locator, timeout, index).click()
        except (ElementClickInterceptedException, ElementNotInteractableException):
            web_element = self.driver.find_elements(*locator)[index]
            try:
                self.scroll_to_element(web_element)
                web_element.click()
            except (ElementClickInterceptedException, ElementNotInteractableException):
                self.click_using_javascript(web_element)

    def click_using_javascript(self, element):
        """
        Clicks a WebElement through JavaScript.
        Useful for items hidden below a banner after scrolling, among other scenarios.
        :param element: A Selenium `WebElement`
        """
        self.driver.execute_script("arguments[0].click();", element)

    def is_element_present(self, locator):
        """
        Returns true if the element at the specified locator is present in the
        browser.
        """
        self.driver.implicitly_wait(self.global_timeout)
        try:
            return expected_conditions.presence_of_element_located(locator)(self.driver)
        except NoSuchElementException:
            return False

    def find_element(self, locator, timeout=global_timeout, index=0):
        """
        This method find the element to interact with it with driver methods
        :param locator:     The element locator to find.
        :param timeout:     The time to wait for the element be visible.
                            By default is 'global_timeout'.
        :param index:       If locator returns only one element, index should be 0.
                            If locator returns more than one element, index should be > 0.
        :return:            The element found with the locator.
        :exception TimeoutException informing what element was waiting for.
        """
        self.wait_for_visibility_of_element(locator, timeout, index)

        return self.driver.find_elements(*locator)[index]

    def find_clickable_element(self, locator, timeout=global_timeout, index=0):
        """
        This method find the element to be clicked with it.
        :param locator:     The element locator to find.
        :param timeout:     The time to wait for the element be visible.
                            By default is 'global_timeout'.
        :param index:       If locator returns only one element, index should be 0.
                            If locator returns more than one element, index should be > 0.
        :return:            The element found with the locator.
        :exception TimeoutException informing what element was waiting for.
        """
        self.wait_for_element_to_be_clickable(locator, timeout)
        elements = self.driver.find_elements(*locator)

        if not len(elements) > 0:
            raise ElementNotInteractableException(
                f'The following element/s is/are not able to be interactable: {locator}'
            )

        return elements[index]

    def find_elements(self, locator, timeout=global_timeout):
        """
        Find elements given a By strategy and locator.
        :param locator:     The locator to find the elements.
        :param timeout:     The time to wait for the element be visible.
                            By default is 'global_timeout'.
        :return:            List of web element
        """
        by, value = locator
        self.wait_for_visibility_of_element((by, value), timeout, 0)
        return self.driver.find_elements(by, value)

    def find_clickable_elements(self, by=By.ID, value=None, timeout=global_timeout):
        """
        This method find elements to be clicked with it.
        :param by:          By strategy used for search
        :param value:       String locator to search for.
        :param timeout:     The time to wait for the element be visible.
                            By default is 'global_timeout'.
        :return:            List of web element
        """
        self.wait_for_element_to_be_clickable((by, value), timeout)
        return self.driver.find_elements(by, value)

    def check_element_is_visible(self, locator, timeout=global_timeout):
        """
        This method waits for visibility of an element. If the element after wait is not visible
        TimeOutException is thrown
        :param locator: the element locator to find
        :param timeout: the time to wait for the element be visible. By default is 'global_timeout'
        :return: True is the element is found
        :exception TimeoutException informing what element was waiting for.
        """
        error_msg = f'Element {str(locator)} is not visible after {timeout} seconds'
        self.driver.implicitly_wait(self.global_timeout)

        try:
            ignored_exceptions = [NoSuchElementException,
                                  StaleElementReferenceException,
                                  ElementNotVisibleException]
            self.wait_until(
                lambda s: expected_conditions.visibility_of_element_located(locator)(self.driver),
                timeout, error_msg, ignored_exceptions
            )
            return True
        except TimeoutException:
            logger.info(error_msg)
            return False

    def check_element_is_not_visible(self, locator, timeout=global_timeout):
        """
        Checks that an element is either invisible or not present on the DOM.
        :param locator: Locator     Element locator to find.
        :param timeout: Int         Timeout.
        :return:        Boolean     True if the element is not visible, False otherwise.
        """
        error_msg = 'Element is still visible after {0} seconds'.format(str(timeout))
        self.driver.implicitly_wait(self.global_timeout)

        try:
            result = self.wait_until(
                lambda s: expected_conditions.invisibility_of_element_located(locator)(
                    self.driver),
                timeout, error_msg, [WebDriverException])
            if isinstance(result, WebElement):
                return True
            elif isinstance(result, bool):
                return result
        except TimeoutException:
            logger.info(error_msg)
            return False

    def _is_element_visible(self, locator, index=0):
        """
        Returns true if the element at the specified locator is visible in the
        browser at the given index from locator
        Note: It uses an implicit wait if it cannot find the element
        immediately.
        """
        try:
            if index == 0:
                return expected_conditions.visibility_of_element_located(locator)(self.driver)
            elif index > 0:
                return self._get_element(locator, index).is_displayed()
            else:
                raise ValueError("Index must be an integer greater or equal to 0")
        except NoSuchElementException:
            return False

    def is_element_clickable(self, locator):
        """
        Checks if element is available to be clicked (it is Displayed and Enabled).
        :param locator: Locator     Element locator.
        :return:        Boolean     True if element is clickable, false otherwise.
        """
        try:
            return expected_conditions.element_to_be_clickable(locator)(self.driver)
        except (NoSuchElementException, ElementNotVisibleException):
            return False

    def wait_for_element_to_be_clickable(self, locator, timeout=global_timeout):
        """
        This method waits for an element to be clickable.
        If the element after wait is not clickable TimeOutException is thrown.
        :param locator: the element locator to find.
        :param timeout: the time to wait for the element be clickable.
                        By default is 'global_timeout'.
        :return: True is the element is clickable.
        :exception TimeoutException informing what element was waiting for.
        """
        error_msg = f'Element {str(locator)} is not clickable after {timeout} seconds'
        self.wait_until(
            lambda s: expected_conditions.element_to_be_clickable(locator), timeout, error_msg
        )

    def wait_for_visibility_of_element(self, locator, timeout=global_timeout, index=0):
        """
        This method waits for visibility of an element. If the element after wait
        is not visible TimeOutException is thrown
        :param locator:     The element locator to find.
        :param timeout:     The time to wait for the element be visible.
                            By default is 'global_timeout'
        :param index:       If locator returns only one element, index should be 0.
                            If locator returns more than one element, index should be > 0.
        :return: the element found with the locator
        :exception TimeoutException informing what element was waiting for.
        """
        self.wait_until(
            lambda s: self._is_element_visible(locator, index),
            timeout,
            f"Element {str(locator)} is not visible after {timeout} seconds."
        )

    def wait_for_visibility_of_elements(self, locators, timeout=global_timeout):
        """
        This method waits for visibility of a list of elements. If an element
        after wait is not visible TimeOutException is thrown
        :param locators: a list of locators to find
        :param timeout: the time to wait for the element be visible.
        By default is 'global_timeout'
        :return: None
        :exception TimeoutException informing what element was waiting for.
        """
        for locator in locators:
            self.wait_for_visibility_of_element(locator, timeout)

    def wait_until(self, function, timeout, timeout_msg_error='', ignored_exceptions=None):
        """
        Waits till driver completes a function during a timeout period.
        :param function:            Function    Function to be executed.
        :param timeout:             Int         Timeout.
        :param timeout_msg_error:   String      Message to show if timeout is exceeded.
        :param ignored_exceptions:  List        A list with exception classes ignored during calls.
        :return:                    Mixed       Function return value.
        """
        if timeout is None:
            timeout = self.get_default_timeout()

        wait = WebDriverWait(self.driver, timeout, ignored_exceptions=ignored_exceptions)
        value = wait.until(lambda s: function(self), timeout_msg_error)
        return value

    def wait_for_presence_of_element(self, locator, timeout=global_timeout):
        """
        Waits until an element is present.
        :param locator:     Locator     Locator of element.
        :param timeout:     Int         Timeout
        """
        self.wait_until(
            lambda s: s.check_element_is_visible(locator, timeout), timeout,
            "Element {} is not present after {} seconds.".format(str(locator), timeout)
        )

    def wait_and_scroll_to_element(self, locator, timeout=global_timeout):
        """
        Waits and scroll to element given its locator.
        :param timeout: the time to wait if the element appears. By default is global_timeout
        :param locator: locator of element to scroll
        """
        self.wait_for_presence_of_element(locator, timeout)
        element = self.driver.find_element(*locator)
        self.scroll_to_element(element)

    def scroll_to_element(
            self, web_element, options='{block: "center", inline: "center", behavior: "smooth"}'
    ):
        """
        Given a WebElement, scrolls the page to its current position.
        This function uses selenium function to scroll to element. The benefits can be read here
        https://stackoverflow.com/questions/34562095/scrollintoview-vs-movetoelement
        :param web_element:     WebElement      Chosen element on the view.
        :param options:         String          Scrolling options.
        """
        self.driver.execute_script(f'arguments[0].scrollIntoView({options});', web_element)
        actions = ActionChains(self.driver)
        actions.move_to_element(web_element).perform()

    def get_cookie_named(self, cookie_name):
        cookie = self.driver.get_cookie(cookie_name)

        if not cookie:
            raise ValueError('Cookie named "{}" was not found.'.format(cookie_name))

        return cookie

    def set_cookie(self, cookie_attrs):
        self.driver.add_cookie(cookie_attrs)

    def switch_to_next_tab(self, timeout=global_timeout):
        """
        Switch to the next navigator tab.
        """

        def evaluate_windows_handles(current_handle):
            handlers = self.driver.window_handles
            current_window_index = handlers.index(current_handle)
            try:
                next_handler = self.driver.window_handles[current_window_index + 1]
            except IndexError:
                next_handler = None

            return next_handler

        current_windows_handle = self.driver.current_window_handle
        wanted_handler = poll_data(
            current_windows_handle, evaluate_windows_handles, float(timeout)
        )

        if not wanted_handler:
            raise TimeoutException("The window couldn't be opened")
        else:
            self.driver.switch_to.window(wanted_handler)

    def get_active_element(self):
        return self.driver.switch_to.active_element

    def is_browser_alert_present(self, timeout):
        is_alert_present = True

        try:
            self.wait_until(
                lambda s: expected_conditions.alert_is_present()(self.driver), timeout
            )
        except TimeoutException:
            is_alert_present = False

        return is_alert_present

    def get_browser_alert(self):
        return self.driver.switch_to.alert