#include "vulkan_helpers.hpp"

#include "asserts.hpp"

auto get_layer_properties() -> std::vector<VkLayerProperties> {
    VkResult vk_result{VK_SUCCESS};

    std::uint32_t                  count{0};
    std::vector<VkLayerProperties> properties;

    vk_result = vkEnumerateInstanceLayerProperties(&count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed getting layer properties count");

    if(count) {
        properties.resize(count);
        vk_result = vkEnumerateInstanceLayerProperties(&count, properties.data());
        assert_eq(vk_result, VK_SUCCESS, "failed getting layer properties");
    }

    return properties;
}

auto get_extension_names(SDL_Window* window) -> std::vector<char const*> {
    int sdl_result{SDL_TRUE};

    std::uint32_t            count{0};
    std::vector<char const*> names;

    sdl_result = SDL_Vulkan_GetInstanceExtensions(window, &count, nullptr);
    sdl_assert_eq(sdl_result, SDL_TRUE);

    if(count) {
        names.resize(count);
        sdl_result = SDL_Vulkan_GetInstanceExtensions(window, &count, names.data());
        sdl_assert_eq(sdl_result, SDL_TRUE);
    }

    return names;
}

auto get_physical_devices(VkInstance vk_instance) -> std::vector<VkPhysicalDevice> {
    VkResult vk_result{VK_SUCCESS}
    ;
    std::uint32_t                 count{0};
    std::vector<VkPhysicalDevice> devices;

    //
    vk_result = vkEnumeratePhysicalDevices(vk_instance, &count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed getting device count");

    if(count) {
        devices.resize(count);

        vk_result = vkEnumeratePhysicalDevices(vk_instance, &count, devices.data());
        assert_eq(vk_result, VK_SUCCESS, "failed getting device count");
    }

    return devices;
}

auto get_physical_device_extension_properties(VkPhysicalDevice device)
    -> std::vector<VkExtensionProperties>
{
    VkResult vk_result{VK_SUCCESS};

    std::uint32_t                      count{0};
    std::vector<VkExtensionProperties> props;

    vk_result = vkEnumerateDeviceExtensionProperties(device, nullptr, &count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed to queue physical extension properties count");

    if(count) {
        props.resize(count);
        vk_result = vkEnumerateDeviceExtensionProperties(device, nullptr, &count, props.data());
        assert_eq(vk_result, VK_SUCCESS, "failed to queue physical extension properties");
    }

    return props;
}

auto get_queue_family_properties(VkPhysicalDevice device) 
    -> std::vector<VkQueueFamilyProperties>
{
    std::uint32_t                        count{0};
    std::vector<VkQueueFamilyProperties> props;

    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, nullptr);

    if(count) {
        props.resize(count);
        vkGetPhysicalDeviceQueueFamilyProperties(device, &count, props.data());
    }

    return props;
}

auto get_physical_device_surface_formats(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> std::vector<VkSurfaceFormatKHR>
{
    VkResult vk_result{VK_SUCCESS};

    std::uint32_t                   count{0};
    std::vector<VkSurfaceFormatKHR> formats;

    vk_result = vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed getting physical device surface formats count");

    if(count) {
        formats.resize(count);
        vk_result = vkGetPhysicalDeviceSurfaceFormatsKHR(device, surface, &count, formats.data());
        assert_eq(vk_result, VK_SUCCESS, "failed getting physical device surface formats");
    }

    return formats;
}

auto get_physical_device_present_modes(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> std::vector<VkPresentModeKHR>
{
    VkResult vk_result{VK_SUCCESS};

    std::uint32_t                 count{0};
    std::vector<VkPresentModeKHR> modes;

    vk_result = vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed getting physical device surface present modes count");

    if(count) {
        modes.resize(count);
        vk_result = vkGetPhysicalDeviceSurfacePresentModesKHR(device, surface, &count, modes.data());
        assert_eq(vk_result, VK_SUCCESS, "failed getting physical device surface present modes");
    }

    return modes;
}

auto get_swap_chain_details(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> SwapChainDetails 
{
    VkResult vk_result{VK_SUCCESS};

    SwapChainDetails swap_chain_details;

    vk_result = vkGetPhysicalDeviceSurfaceCapabilitiesKHR( device
                                                         , surface
                                                         , &swap_chain_details.caps );
    assert_eq(vk_result, VK_SUCCESS, "failed to get physical device surface caps");

    swap_chain_details.formats = get_physical_device_surface_formats(device, surface);
    swap_chain_details.present_modes = get_physical_device_present_modes(device, surface);

    return swap_chain_details;
}

auto create_shader_module(VkDevice device, const std::vector<char> &binary) -> VkShaderModule {
    VkShaderModuleCreateInfo create_info {};
    create_info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
    create_info.pNext    = nullptr;
    create_info.flags    = 0;
    create_info.codeSize = binary.size();
    create_info.pCode    = reinterpret_cast<std::uint32_t const*>(binary.data());

    VkShaderModule module_{VK_NULL_HANDLE};
    auto const vk_result = vkCreateShaderModule(device, &create_info, nullptr, &module_);
    assert_eq(vk_result, VK_SUCCESS, "failed to create shader module");

    return module_;
}
