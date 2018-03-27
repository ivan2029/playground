#include "app.hpp"

#include <iostream>
#include <iomanip>
#include <vector>
#include <string>
#include <string_view>
#include <stdexcept>
#include <algorithm>
#include <optional>
#include <set>
using namespace std::literals;

#include "scoped_guard.hpp"
#include "asserts.hpp"
#include "vulkan_helpers.hpp"


//
//
//
std::vector<char const*> LAYER_NAMES {
    "VK_LAYER_LUNARG_standard_validation"
};

std::vector<char const*> DEVICE_EXTENSIONS {
    VK_KHR_SWAPCHAIN_EXTENSION_NAME
};

static 
auto check_layer_property(VkLayerProperties const& prop) -> bool {
    auto const predicate = [&](auto const& name) {
        return std::strcmp(name, prop.layerName) == 0;
    };
    return std::any_of(LAYER_NAMES.begin(), LAYER_NAMES.end(), predicate);
}

static
auto check_layer_properties (std::vector<VkLayerProperties> const& props) -> bool 
{
    return std::any_of(props.begin(), props.end(), check_layer_property);
}

//
//
//
static
auto check_device_extensions(VkPhysicalDevice device) -> bool {
    auto const props = get_physical_device_extension_properties(device);

    //
    std::vector<std::string> available_ext;
    std::transform( props.begin(), props.end()
                  , std::back_inserter(available_ext)
                  , [](VkExtensionProperties const& p) { 
                        return std::string{p.extensionName}; 
                    });
    std::sort(available_ext.begin(), available_ext.end());
    
    //
    std::vector<std::string> required_ext(DEVICE_EXTENSIONS.begin(), DEVICE_EXTENSIONS.end());
    std::sort(required_ext.begin(), required_ext.end());

    //
    return std::includes( available_ext.begin(), available_ext.end()
                        , required_ext.begin(), required_ext.end() );
}

static
auto check_formats_and_present_modes(VkPhysicalDevice device, VkSurfaceKHR surface)
    -> bool
{
    auto const details = get_swap_chain_details(device, surface);

    auto const format_it =
            std::find_if( details.formats.begin(), details.formats.end()
                        , [](VkSurfaceFormatKHR const& format) {
                              return format.format == VK_FORMAT_B8G8R8A8_UNORM
                                  && format.colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
                                  ;
                        });
    auto const format = details.formats.end() != format_it;

    auto const present_mode_it =
            std::find( details.present_modes.begin(), details.present_modes.end()
                     , VK_PRESENT_MODE_FIFO_KHR );
    auto const present_mode = details.present_modes.end() != present_mode_it;

    return format && present_mode;
}

//
//
//
App::App(int, char**) {
}

App::~App() {
}

auto App::run() -> void {
    //
    int const sdl_init_result = SDL_Init(SDL_INIT_VIDEO);
    sdl_assert_eq(sdl_init_result, 0);
    SCOPE_GUARD( SDL_Quit(); );
    
    //
    init_window();
    SCOPE_GUARD( deinit_window(); );

    //
    vlk_create_instance();
    SCOPE_GUARD( vlk_destroy_instance(); );

    //
    vlk_create_surface();
    SCOPE_GUARD( vlk_destroy_surface(); );

    //
    vlk_pick_physical_device();
    
    vlk_print_physical_device_details();

    vlk_create_device();
    SCOPE_GUARD( vlk_destroy_device(); );

    vlk_get_graphics_queue();
    vlk_get_present_queue();

    vlk_create_swap_chain();
    SCOPE_GUARD( vlk_destroy_swap_chain(); );

    vlk_get_swap_chain_images();

    //
    main_loop();
}

auto App::main_loop() -> void {
    while(running) {
        process_events();
        
        // clear screen

        // draw
        // TODO

        // swap buffers

    }
}

auto App::process_events() -> void {
    SDL_Event event;
    while( SDL_PollEvent(&event) ) {
        if(event.type == SDL_QUIT) {
            running = false;
        }
        else if(event.type == SDL_KEYDOWN) {
            if(event.key.keysym.sym == SDLK_ESCAPE) {
                running = false;
            }
        }
        else if(event.type == SDL_KEYUP) {
        }
    }
}

auto App::init_window() -> void {
    window = 
        SDL_CreateWindow( "Vulkan"
                        , SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED
                        , screen_width, screen_height
                        , SDL_WINDOW_VULKAN 
        );
    sdl_assert_not_eq(window, nullptr);
}

auto App::deinit_window() -> void {
    if(window == nullptr) return;
    SDL_DestroyWindow(window);
}

auto App::vlk_create_instance() -> void {
    //
    auto extension_names = get_extension_names(window);

    std::cout << "Available extensions:\n";
    for(auto const& name: extension_names) {
        std::cout << "    " << name << "\n";
    }
    std::cout << "\n";

    // check for validation layer
    auto layer_properties = get_layer_properties();

    vlk_validation_layer_enabled = check_layer_properties(layer_properties);

    std::cout << "Available layers:\n";
    for(auto const& lp: layer_properties) {
        std::cout << "    " << lp.layerName << "\n";
    }
    std::cout << "\n";

    //
    VkApplicationInfo app_info {};
    app_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    app_info.pNext              = nullptr;
    app_info.pApplicationName   = "Hello, Vulkan";
    app_info.applicationVersion = VK_MAKE_VERSION(1, 0, 0);
    app_info.pEngineName        = "no engine";
    app_info.engineVersion      = VK_MAKE_VERSION(1, 0, 0);
    app_info.apiVersion         = VK_API_VERSION_1_0;

    VkInstanceCreateInfo create_info {};
    create_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    create_info.pNext                   = nullptr;
    create_info.flags                   = 0;
    create_info.pApplicationInfo        = &app_info;
    if(vlk_validation_layer_enabled) {
        create_info.enabledLayerCount       = static_cast<std::uint32_t>(LAYER_NAMES.size());
        create_info.ppEnabledLayerNames     = LAYER_NAMES.data();
    }
    else {
        create_info.enabledLayerCount       = 0;
        create_info.ppEnabledLayerNames     = nullptr;
    }
    create_info.enabledExtensionCount   = static_cast<std::uint32_t>(extension_names.size());
    create_info.ppEnabledExtensionNames = extension_names.data();

    //
    auto vk_result = vkCreateInstance(&create_info, nullptr, &vlk_instance);
    assert_eq(vk_result, VK_SUCCESS, "vulkan instance not created");
}

auto App::vlk_destroy_instance() -> void {
    vkDestroyInstance(vlk_instance, nullptr);
}

auto App::vlk_create_surface() -> void {
    auto const result = 
        SDL_Vulkan_CreateSurface(window, vlk_instance, &vlk_surface);
    sdl_assert_eq(result, SDL_TRUE);
}

auto App::vlk_destroy_surface() -> void {
    vkDestroySurfaceKHR(vlk_instance, vlk_surface, nullptr);
}

auto App::vlk_find_queue_families(VkPhysicalDevice device) -> QueueFamilyIndices {
    QueueFamilyIndices indices {};

    auto const queue_props = get_queue_family_properties(device);

    for(int i{0}; i < static_cast<int>(queue_props.size()); ++ i) {
        if( !(queue_props[i].queueCount > 0) ) continue;
        // queue has graphics capabilities
        if( queue_props[i].queueFlags & VK_QUEUE_GRAPHICS_BIT ) {
            indices.graphics = i;
        }
        // queue can present to our surface
        VkBool32 present{VK_FALSE};
        auto const vk_result = 
            vkGetPhysicalDeviceSurfaceSupportKHR(device, i, vlk_surface, &present);
        if(vk_result == VK_SUCCESS && present == VK_TRUE) {
            indices.present = i;
        }
        //
        if(indices.is_complete()) break;
    }

    return indices;
}

auto App::vlk_pick_physical_device() -> void {
    auto const pds = get_physical_devices(vlk_instance);

    auto const predicate = [&](VkPhysicalDevice p) {
        return check_device_extensions(p)
            && check_formats_and_present_modes(p, vlk_surface)
            && vlk_find_queue_families(p).is_complete()
            ;
    };

    auto const it = std::find_if(pds.begin(), pds.end(), predicate);
    assert_not_eq(it, pds.end(), "suitable device not found");

    vlk_physical_device = *it;
}

auto App::vlk_print_physical_device_details() -> void {
    //
    VkPhysicalDeviceProperties properties {};
    vkGetPhysicalDeviceProperties(vlk_physical_device, &properties);
    
    auto const version_major = VK_VERSION_MAJOR(properties.apiVersion);
    auto const version_minor = VK_VERSION_MINOR(properties.apiVersion);
    auto const version_patch = VK_VERSION_PATCH(properties.apiVersion);

    auto const version_string = 
        std::to_string(version_major) + "." +
        std::to_string(version_minor) + "." +
        std::to_string(version_patch)
        ;

    std::cout 
        << "Picked device:\n"
        << "    Device name: " << properties.deviceName << "\n"
        << "      Device ID: " << properties.deviceID   << "\n"
        << "    API version: " << version_string        << "\n"
        << "      Vendor ID: " << properties.vendorID   << "\n"
        ;

    //
    auto const ext_props = get_physical_device_extension_properties(vlk_physical_device);

    std::cout << "\nDevice extensions:\n";
    for(auto const& p: ext_props) {
        std::cout << "    " << p.extensionName << "\n";
    }
    std::cout << "\n";

    //
    auto const details = get_swap_chain_details(vlk_physical_device, vlk_surface);

    std::cout << "Formats:\n";
    for(auto const& f: details.formats) {
        std::cout << "    "
                  << "format = " << std::setw(8) << f.format
                  << ", color space = " << std::setw(8) << f.colorSpace
                  << "\n";
    }
    std::cout << "\n";

    std::cout << "Present modes:\n";
    for(auto const& m: details.present_modes) {
        std::cout << "    " << m << "\n";
    }
    std::cout << "\n";

}

auto App::vlk_create_device() -> void {
    auto const indices = vlk_find_queue_families(vlk_physical_device);

    std::set<int> unique_queue_families{indices.graphics, indices.present};

    float queue_priority {1.0f};
    std::vector<VkDeviceQueueCreateInfo> queue_create_infos;
    for(auto family: unique_queue_families) {
        VkDeviceQueueCreateInfo queue_create_info {};
        queue_create_info.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
        queue_create_info.pNext            = nullptr;
        queue_create_info.flags            = 0;
        queue_create_info.queueFamilyIndex = family;
        queue_create_info.queueCount       = 1;
        queue_create_info.pQueuePriorities = &queue_priority;

        queue_create_infos.push_back(queue_create_info);
    }
    
    VkPhysicalDeviceFeatures device_features {};

    auto const device_exts = get_physical_device_extension_properties(vlk_physical_device);

    VkDeviceCreateInfo device_create_info {};
    device_create_info.sType                = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    device_create_info.pNext                = nullptr;
    device_create_info.flags                = 0;
    device_create_info.queueCreateInfoCount = static_cast<std::uint32_t>(queue_create_infos.size());
    device_create_info.pQueueCreateInfos    = queue_create_infos.data();
    if(vlk_validation_layer_enabled) {
        device_create_info.enabledLayerCount   = static_cast<std::uint32_t>(LAYER_NAMES.size());
        device_create_info.ppEnabledLayerNames = LAYER_NAMES.data();
    }
    else {
        device_create_info.enabledLayerCount   = 0;
        device_create_info.ppEnabledLayerNames = nullptr;
    }
    device_create_info.enabledExtensionCount   = static_cast<std::uint32_t>(DEVICE_EXTENSIONS.size());
    device_create_info.ppEnabledExtensionNames = DEVICE_EXTENSIONS.data();
    device_create_info.pEnabledFeatures        = &device_features;

    auto const vk_result = vkCreateDevice( vlk_physical_device
                                         , &device_create_info
                                         , nullptr
                                         , &vlk_device );
    assert_eq(vk_result, VK_SUCCESS, "failed to create device");
}

auto App::vlk_destroy_device() -> void {
    vkDestroyDevice(vlk_device, nullptr);
}

auto App::vlk_get_graphics_queue() -> void {
    auto const indices = vlk_find_queue_families(vlk_physical_device);
    vkGetDeviceQueue(vlk_device, indices.graphics, 0, &vlk_graphics_queue);
}

auto App::vlk_get_present_queue() -> void {
    auto const indices = vlk_find_queue_families(vlk_physical_device);
    vkGetDeviceQueue(vlk_device, indices.present, 0, &vlk_present_queue);
}

auto App::vlk_create_swap_chain() -> void {
    //
    auto const details = get_swap_chain_details(vlk_physical_device, vlk_surface);
    auto const indices = vlk_find_queue_families(vlk_physical_device);

    std::uint32_t queue_family_indices[] = {
        static_cast<std::uint32_t>(indices.graphics),
        static_cast<std::uint32_t>(indices.present)
    };

    VkExtent2D extent;

    extent.width = std::clamp( static_cast<std::uint32_t>(screen_width)
                             , details.caps.minImageExtent.width
                             , details.caps.maxImageExtent.width );

    extent.height = std::clamp( static_cast<std::uint32_t>(screen_height)
                              , details.caps.minImageExtent.height
                              , details.caps.maxImageExtent.height );

    //
    VkSwapchainCreateInfoKHR create_info{};

    create_info.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.surface = vlk_surface;
    create_info.minImageCount = details.caps.minImageCount;
    create_info.imageFormat = VK_FORMAT_B8G8R8A8_UNORM;
    create_info.imageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
    create_info.imageExtent = extent;
    create_info.imageArrayLayers = 1;
    create_info.imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;

    if(indices.graphics != indices.present) {
        create_info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        create_info.queueFamilyIndexCount = 2;
        create_info.pQueueFamilyIndices = queue_family_indices;
    }
    else {
        create_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
        create_info.queueFamilyIndexCount = 0;
        create_info.pQueueFamilyIndices = nullptr;
    }

    create_info.preTransform = details.caps.currentTransform;
    create_info.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
    create_info.presentMode = VK_PRESENT_MODE_FIFO_KHR;
    create_info.clipped = VK_TRUE;
    create_info.oldSwapchain = VK_NULL_HANDLE;

    //
    auto const vk_result = vkCreateSwapchainKHR(vlk_device, &create_info, nullptr, &vlk_swap_chain);
    assert_eq(vk_result, VK_SUCCESS, "failed to create swap chain");

    vlk_extent = extent;
    vlk_format = VK_FORMAT_B8G8R8A8_UNORM;
}

auto App::vlk_destroy_swap_chain() -> void {
    vkDestroySwapchainKHR(vlk_device, vlk_swap_chain, nullptr);
}

auto App::vlk_get_swap_chain_images() -> void {
    VkResult vk_result{VK_SUCCESS};
    std::uint32_t         count{0};
    std::vector<VkImage>  images;

    vk_result = vkGetSwapchainImagesKHR(vlk_device, vlk_swap_chain, &count, nullptr);
    assert_eq(vk_result, VK_SUCCESS, "failed to get swap chain image count");

    if(count) {
        images.resize(count);
        vk_result = vkGetSwapchainImagesKHR(vlk_device, vlk_swap_chain, &count, images.data());
        assert_eq(vk_result, VK_SUCCESS, "failed to get swap chain images");
    }

    vlk_images = std::move(images);
}
