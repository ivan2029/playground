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
#include "read_file_contents.hpp"

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
    init_vulkan();
    SCOPE_GUARD( deinit_vulkan(); );

    //
    main_loop();

    //
    vkDeviceWaitIdle(vlk_device);
}

auto App::main_loop() -> void {
    while(running) {
        process_events();
        draw_frame();
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

auto App::draw_frame() -> void {
    VkResult vk_result{VK_SUCCESS};

    //
    vkQueueWaitIdle(vlk_present_queue);

    //
    std::uint32_t image_index;
    vkAcquireNextImageKHR( vlk_device
                         , vlk_swap_chain
                         , std::numeric_limits<std::uint64_t>::max()
                         , vlk_image_available_sem
                         , VK_NULL_HANDLE
                         , &image_index  );

    //
    VkPipelineStageFlags stage_flags[] = {
      VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    };

    VkSubmitInfo submit_info {};
    submit_info.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;
    submit_info.pNext = nullptr;
    submit_info.waitSemaphoreCount = 1;
    submit_info.pWaitSemaphores    = &vlk_image_available_sem;
    submit_info.pWaitDstStageMask  = stage_flags;
    submit_info.commandBufferCount = 1;
    submit_info.pCommandBuffers    = &vlk_command_buffers[image_index];
    submit_info.signalSemaphoreCount = 1;
    submit_info.pSignalSemaphores    = &vlk_render_finished_sem;

    vk_result = vkQueueSubmit(vlk_graphics_queue, 1, &submit_info, VK_NULL_HANDLE);
    assert_eq(vk_result, VK_SUCCESS, "failed to submit drawing buffer");

    //
    VkPresentInfoKHR present_info {};
    present_info.sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
    present_info.pNext = nullptr;
    present_info.waitSemaphoreCount = 1;
    present_info.pWaitSemaphores    = &vlk_render_finished_sem;
    present_info.swapchainCount     = 1;
    present_info.pSwapchains        = &vlk_swap_chain;
    present_info.pImageIndices      = &image_index;
    present_info.pResults           = nullptr;

    //
    vkQueuePresentKHR(vlk_present_queue, &present_info);
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

auto App::init_vulkan() -> void {
    vlk_create_instance();
    vlk_create_surface();
    vlk_pick_physical_device();

    vlk_print_physical_device_details();

    vlk_create_device();
    vlk_get_graphics_queue();
    vlk_get_present_queue();
    vlk_create_swap_chain();
    vlk_get_swap_chain_images();
    vlk_get_swap_chain_image_views();

    vlk_print_image_info();

    vlk_create_pipeline();
    vlk_create_framebuffers();
    vlk_create_command_pool();
    vlk_create_command_buffers();
    vlk_create_semaphores();
}

auto App::deinit_vulkan() -> void {
    SCOPE_GUARD( vlk_destroy_instance(); );
    SCOPE_GUARD( vlk_destroy_surface(); );
    SCOPE_GUARD( vlk_destroy_device(); );
    SCOPE_GUARD( vlk_destroy_swap_chain(); );
    SCOPE_GUARD( vlk_destroy_swap_chain_image_views(); );
    SCOPE_GUARD( vlk_destroy_pipeline(); );
    SCOPE_GUARD( vlk_destroy_framebuffers(); );
    SCOPE_GUARD( vlk_destroy_command_pool(); );
    SCOPE_GUARD( vlk_destroy_semaphores(); );
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

auto App::vlk_get_swap_chain_image_views() -> void {
    std::vector<VkImageView> image_views;
    image_views.reserve(vlk_images.size());

    auto const get_image_view = [&](VkImage const image) -> VkImageView {
        VkImageViewCreateInfo create_info {};
        create_info.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
        create_info.image = image;
        create_info.viewType = VK_IMAGE_VIEW_TYPE_2D;
        create_info.format = vlk_format;
        create_info.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
        create_info.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
        create_info.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
        create_info.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
        create_info.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
        create_info.subresourceRange.baseMipLevel = 0;
        create_info.subresourceRange.levelCount = 1;
        create_info.subresourceRange.baseArrayLayer = 0;
        create_info.subresourceRange.layerCount = 1;

        VkImageView view{};

        auto const vk_result = vkCreateImageView(vlk_device, &create_info, nullptr, &view);
        assert_eq(vk_result, VK_SUCCESS, "failed to create image view");

        return view;
    };

    std::transform(vlk_images.begin(), vlk_images.end()
                  , std::back_inserter(image_views)
                  , get_image_view );

    vlk_image_views = std::move(image_views);
}

auto App::vlk_destroy_swap_chain_image_views() -> void {
    for(auto const view: vlk_image_views) {
        vkDestroyImageView(vlk_device, view, nullptr);
    }
}

auto App::vlk_print_image_info() -> void {
    std::cout << "Image view count: " << vlk_image_views.size() << "\n";
    std::cout << "\n";
}

auto App::vlk_create_pipeline() -> void {
    vlk_create_shader_modules();
    vlk_create_pipeline_layout();
    vlk_create_render_pass();
    vlk_create_pipeline_object();
}

auto App::vlk_destroy_pipeline() -> void {
    vlk_destroy_shader_modules();
    vlk_destroy_pipeline_layout();
    vlk_destroy_render_pass();
    vlk_destroy_pipeline_object();
}

auto App::vlk_create_shader_modules() -> void {
    auto const vert_spv = read_file_contents("shaders/vert.spv");
    auto const frag_spv = read_file_contents("shaders/frag.spv");

    auto const vert_mod = create_shader_module(vlk_device, vert_spv);
    auto const frag_mod = create_shader_module(vlk_device, frag_spv);

    vlk_vertex_shader_module   = vert_mod;
    vlk_fragment_shader_module = frag_mod;
}

auto App::vlk_destroy_shader_modules() -> void {
    vkDestroyShaderModule(vlk_device,   vlk_vertex_shader_module, nullptr);
    vkDestroyShaderModule(vlk_device, vlk_fragment_shader_module, nullptr);
}

auto App::vlk_create_pipeline_layout() -> void {
    VkPipelineLayout layout;

    VkPipelineLayoutCreateInfo layout_create_info {};
    layout_create_info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
    layout_create_info.pNext = nullptr;
    layout_create_info.flags = 0;
    layout_create_info.setLayoutCount         = 0;
    layout_create_info.pSetLayouts            = nullptr;
    layout_create_info.pushConstantRangeCount = 0;
    layout_create_info.pPushConstantRanges    = nullptr;

    //
    auto const vk_result = vkCreatePipelineLayout(vlk_device, &layout_create_info, nullptr, &layout);
    assert_eq(vk_result, VK_SUCCESS, "failed to create pipeline layout");

    vlk_pipeline_layout = layout;
}

auto App::vlk_destroy_pipeline_layout() -> void {
    vkDestroyPipelineLayout(vlk_device, vlk_pipeline_layout, nullptr);
}

auto App::vlk_create_render_pass() -> void {
    //
    VkAttachmentDescription color_attachment {};
    color_attachment.flags          = 0;
    color_attachment.format         = vlk_format;
    color_attachment.samples        = VK_SAMPLE_COUNT_1_BIT;
    color_attachment.loadOp         = VK_ATTACHMENT_LOAD_OP_CLEAR;
    color_attachment.storeOp        = VK_ATTACHMENT_STORE_OP_STORE;
    color_attachment.stencilLoadOp  = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    color_attachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    color_attachment.initialLayout  = VK_IMAGE_LAYOUT_UNDEFINED;
    color_attachment.finalLayout    = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

    //
    VkAttachmentReference color_attachment_reference {};
    color_attachment_reference.attachment = 0;
    color_attachment_reference.layout     = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

    //
    VkSubpassDescription subpass_description {};
    subpass_description.flags = 0;
    subpass_description.pipelineBindPoint       = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass_description.inputAttachmentCount    = 0;
    subpass_description.pInputAttachments       = nullptr;
    subpass_description.colorAttachmentCount    = 1;
    subpass_description.pColorAttachments       = &color_attachment_reference;
    subpass_description.pResolveAttachments     = nullptr;
    subpass_description.pDepthStencilAttachment = nullptr;
    subpass_description.preserveAttachmentCount = 0;
    subpass_description.pPreserveAttachments    = nullptr;

    //
    VkSubpassDependency dependency {};
    dependency.srcSubpass = VK_SUBPASS_EXTERNAL;
    dependency.dstSubpass = 0;
    dependency.srcStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dependency.dstStageMask = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    dependency.srcAccessMask = 0;
    dependency.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
                             | VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                             ;
    dependency.dependencyFlags = 0;

    //
    VkRenderPassCreateInfo create_info {};
    create_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.attachmentCount = 1;
    create_info.pAttachments    = &color_attachment;
    create_info.subpassCount    = 1;
    create_info.pSubpasses      = &subpass_description;
    create_info.dependencyCount = 1;
    create_info.pDependencies   = &dependency;

    //
    VkRenderPass render_pass;
    auto const vk_result = vkCreateRenderPass(vlk_device, &create_info, nullptr, &render_pass);
    assert_eq(vk_result, VK_SUCCESS, "failed to create render pass");

    vlk_render_pass = render_pass;
}

auto App::vlk_destroy_render_pass() -> void {
    vkDestroyRenderPass(vlk_device, vlk_render_pass, nullptr);
}

auto App::vlk_create_pipeline_object() -> void {
    // ... stages ...
    VkPipelineShaderStageCreateInfo  shader_stage_infos[2];

    // ... state infos ...
    VkViewport viewport;
    VkRect2D   scissor;

    VkPipelineColorBlendAttachmentState color_blend_attachment;

//    VkDynamicState dynamic_states[2] = {
//        VK_DYNAMIC_STATE_VIEWPORT,
//        VK_DYNAMIC_STATE_LINE_WIDTH
//    };

    VkPipelineVertexInputStateCreateInfo   vertex_input_state;
    VkPipelineInputAssemblyStateCreateInfo input_assembly_state;
//    VkPipelineTessellationStateCreateInfo  tessellation_state;
    VkPipelineViewportStateCreateInfo      viewport_state;
    VkPipelineRasterizationStateCreateInfo rasterization_state;
    VkPipelineMultisampleStateCreateInfo   multisample_state;
//    VkPipelineDepthStencilStateCreateInfo  depth_stencil_state;
    VkPipelineColorBlendStateCreateInfo    color_blend_state;
//    VkPipelineDynamicStateCreateInfo       dynamic_state;


    // Vertex assembly
    VkPipelineShaderStageCreateInfo vert_stage_create_info {};
    vert_stage_create_info.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    vert_stage_create_info.pNext = nullptr;
    vert_stage_create_info.flags = 0;
    vert_stage_create_info.stage = VK_SHADER_STAGE_VERTEX_BIT;
    vert_stage_create_info.module = vlk_vertex_shader_module;
    vert_stage_create_info.pName = "main";
    vert_stage_create_info.pSpecializationInfo = nullptr;

    VkPipelineShaderStageCreateInfo frag_stage_create_info {};
    frag_stage_create_info.sType = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
    frag_stage_create_info.pNext = nullptr;
    frag_stage_create_info.flags = 0;
    frag_stage_create_info.stage = VK_SHADER_STAGE_FRAGMENT_BIT;
    frag_stage_create_info.module = vlk_fragment_shader_module;
    frag_stage_create_info.pName = "main";
    frag_stage_create_info.pSpecializationInfo = nullptr;

    shader_stage_infos[0] = vert_stage_create_info;
    shader_stage_infos[1] = frag_stage_create_info;

    // Input stage
    vertex_input_state.sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
    vertex_input_state.pNext = nullptr;
    vertex_input_state.flags = 0;
    vertex_input_state.vertexBindingDescriptionCount   = 0;
    vertex_input_state.pVertexBindingDescriptions      = nullptr;
    vertex_input_state.vertexAttributeDescriptionCount = 0;
    vertex_input_state.pVertexAttributeDescriptions    = nullptr;

    //
    input_assembly_state.sType    = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
    input_assembly_state.pNext    = nullptr;
    input_assembly_state.flags    = 0;
    input_assembly_state.topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
    input_assembly_state.primitiveRestartEnable = VK_FALSE;

    // Viewport
    viewport.x        = 0.0f;
    viewport.y        = 0.0f;
    viewport.width    = static_cast<float>(vlk_extent.width);
    viewport.height   = static_cast<float>(vlk_extent.height);
    viewport.minDepth = 0.0f;
    viewport.maxDepth = 1.0f;

    scissor.offset = {0,0};
    scissor.extent = vlk_extent;

    viewport_state.sType = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
    viewport_state.pNext = nullptr;
    viewport_state.flags = 0;
    viewport_state.viewportCount = 1;
    viewport_state.pViewports    = &viewport;
    viewport_state.scissorCount  = 1;
    viewport_state.pScissors     = &scissor;

    // Rasterization
    rasterization_state.sType = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
    rasterization_state.pNext = nullptr;
    rasterization_state.flags = 0;
    rasterization_state.depthClampEnable        = VK_FALSE;
    rasterization_state.rasterizerDiscardEnable = VK_FALSE;
    rasterization_state.polygonMode             = VK_POLYGON_MODE_FILL;
    rasterization_state.cullMode                = VK_CULL_MODE_BACK_BIT;
    rasterization_state.frontFace               = VK_FRONT_FACE_CLOCKWISE;
    rasterization_state.depthBiasEnable         = VK_FALSE;
    rasterization_state.depthBiasConstantFactor = 0.0f;
    rasterization_state.depthBiasClamp          = 0.0f;
    rasterization_state.depthBiasSlopeFactor    = 0.0f;
    rasterization_state.lineWidth               = 1.0f;

    // Multisampling
    multisample_state.sType = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
    multisample_state.pNext = nullptr;
    multisample_state.flags = 0;
    multisample_state.rasterizationSamples  = VK_SAMPLE_COUNT_1_BIT;
    multisample_state.sampleShadingEnable   = VK_FALSE;
    multisample_state.minSampleShading      = 1.0f;
    multisample_state.pSampleMask           = nullptr;
    multisample_state.alphaToCoverageEnable = VK_FALSE;
    multisample_state.alphaToOneEnable      = VK_FALSE;

    // TODO: DepthStencil placeholder

    // Color blending
    color_blend_attachment.blendEnable = VK_FALSE;
    color_blend_attachment.srcColorBlendFactor = VK_BLEND_FACTOR_ONE;
    color_blend_attachment.dstColorBlendFactor = VK_BLEND_FACTOR_ZERO;
    color_blend_attachment.colorBlendOp = VK_BLEND_OP_ADD;
    color_blend_attachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
    color_blend_attachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ZERO;
    color_blend_attachment.alphaBlendOp = VK_BLEND_OP_ADD;
    color_blend_attachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT
                                          | VK_COLOR_COMPONENT_G_BIT
                                          | VK_COLOR_COMPONENT_B_BIT
                                          | VK_COLOR_COMPONENT_A_BIT
                                          ;

    color_blend_state.sType = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
    color_blend_state.pNext = nullptr;
    color_blend_state.flags = 0;
    color_blend_state.logicOpEnable = VK_FALSE;
    color_blend_state.logicOp = VK_LOGIC_OP_COPY;
    color_blend_state.attachmentCount = 1;
    color_blend_state.pAttachments = &color_blend_attachment;
    color_blend_state.blendConstants[0] = 0.0f;
    color_blend_state.blendConstants[1] = 0.0f;
    color_blend_state.blendConstants[2] = 0.0f;
    color_blend_state.blendConstants[3] = 0.0f;

    // Dynamic state
//    dynamic_state.sType = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
//    dynamic_state.pNext = nullptr;
//    dynamic_state.flags = 0;
//    dynamic_state.dynamicStateCount = 2;
//    dynamic_state.pDynamicStates = dynamic_states;

    VkGraphicsPipelineCreateInfo create_info {};
    create_info.sType = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.stageCount = 2;
    create_info.pStages    = shader_stage_infos;
    create_info.pVertexInputState   = &vertex_input_state;
    create_info.pInputAssemblyState = &input_assembly_state;
    create_info.pTessellationState  = nullptr;
    create_info.pViewportState      = &viewport_state;
    create_info.pRasterizationState = &rasterization_state;
    create_info.pMultisampleState   = &multisample_state;
    create_info.pDepthStencilState  = nullptr;
    create_info.pColorBlendState    = &color_blend_state;
    create_info.pDynamicState       = nullptr;
    create_info.layout     = vlk_pipeline_layout;
    create_info.renderPass = vlk_render_pass;
    create_info.subpass    = 0;
    create_info.basePipelineHandle = VK_NULL_HANDLE;
    create_info.basePipelineIndex = -1;

    VkPipeline pipeline;
    auto const vk_result = vkCreateGraphicsPipelines(vlk_device, VK_NULL_HANDLE, 1, &create_info, nullptr, &pipeline);
    assert_eq(vk_result, VK_SUCCESS, "failed to create pipeline");

    vlk_pipeline = pipeline;
}

auto App::vlk_destroy_pipeline_object() -> void {
    vkDestroyPipeline(vlk_device, vlk_pipeline, nullptr);
}

auto App::vlk_create_framebuffers() -> void {
    std::vector<VkFramebuffer> framebuffers;
    auto const transformation = [&](VkImageView const view) {
        VkFramebufferCreateInfo create_info {};
        create_info.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
        create_info.pNext = nullptr;
        create_info.flags = 0;
        create_info.renderPass      = vlk_render_pass;
        create_info.attachmentCount = 1;
        create_info.pAttachments    = &view;
        create_info.width  = vlk_extent.width;
        create_info.height = vlk_extent.height;
        create_info.layers = 1;

        VkFramebuffer framebuffer;
        auto const vk_result = vkCreateFramebuffer(vlk_device, &create_info, nullptr, &framebuffer);
        assert_eq(vk_result, VK_SUCCESS, "failed to create framebuffer");

        return framebuffer;
    };
    try {
        std::transform( vlk_image_views.begin(), vlk_image_views.end()
                      , std::back_inserter(framebuffers)
                      , transformation );
        vlk_framebuffers = std::move(framebuffers);
    }
    catch(std::exception const& ) {
        for(auto const f: framebuffers) {
            vkDestroyFramebuffer(vlk_device, f, nullptr);
        }
        throw;
    }
}

auto App::vlk_destroy_framebuffers() -> void {
    for(auto const framebuffer: vlk_framebuffers) {
        vkDestroyFramebuffer(vlk_device, framebuffer, nullptr);
    }
}

auto App::vlk_create_command_pool() -> void {
    auto const indices = vlk_find_queue_families(vlk_physical_device);

    VkCommandPoolCreateInfo create_info {};
    create_info.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;
    create_info.queueFamilyIndex = indices.graphics;

    VkCommandPool command_pool;
    auto const vk_result = vkCreateCommandPool(vlk_device, &create_info, nullptr, &command_pool);
    assert_eq(vk_result, VK_SUCCESS, "failed to create command pool");

    vlk_command_pool = command_pool;
}

auto App::vlk_destroy_command_pool() -> void {
    vkDestroyCommandPool(vlk_device, vlk_command_pool, nullptr);
}

auto App::vlk_create_command_buffers() -> void {
    VkResult vk_result{VK_SUCCESS};
    //
    std::vector<VkCommandBuffer> buffers(vlk_framebuffers.size());

    VkCommandBufferAllocateInfo alloc_info {};
    alloc_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
    alloc_info.pNext = nullptr;
    alloc_info.commandPool = vlk_command_pool;
    alloc_info.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
    alloc_info.commandBufferCount = static_cast<std::uint32_t>(buffers.size());

    vk_result = vkAllocateCommandBuffers(vlk_device, &alloc_info, buffers.data());
    assert_eq(vk_result, VK_SUCCESS, "failed to create command buufers");

    vlk_command_buffers = std::move(buffers);

    //
    for(std::size_t i{0}; i != vlk_command_buffers.size(); ++ i) {
        //
        VkCommandBufferBeginInfo begin_info {};
        begin_info.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
        begin_info.pNext = nullptr;
        begin_info.flags = VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT;
        begin_info.pInheritanceInfo = nullptr;

        //
        vkBeginCommandBuffer(vlk_command_buffers[i], &begin_info);

        //
        VkClearValue clear_color{ 0.0f, 0.0f, 0.0f, 1.0f };

        VkRenderPassBeginInfo pass_info {};
        pass_info.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
        pass_info.pNext = nullptr;
        pass_info.renderPass = vlk_render_pass;
        pass_info.framebuffer = vlk_framebuffers[i];
        pass_info.renderArea.offset = {0, 0};
        pass_info.renderArea.extent = vlk_extent;
        pass_info.clearValueCount = 1;
        pass_info.pClearValues = &clear_color;

        //
        vkCmdBeginRenderPass(vlk_command_buffers[i], &pass_info, VK_SUBPASS_CONTENTS_INLINE);
        {
            vkCmdBindPipeline(vlk_command_buffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, vlk_pipeline);
            vkCmdDraw(vlk_command_buffers[i], 3, 1, 0, 0);
        }
        vkCmdEndRenderPass(vlk_command_buffers[i]);

        vk_result = vkEndCommandBuffer(vlk_command_buffers[i]);
    }
}

auto App::vlk_create_semaphores() -> void {
    VkResult vk_result{VK_SUCCESS};

    VkSemaphoreCreateInfo create_info {};
    create_info.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
    create_info.pNext = nullptr;
    create_info.flags = 0;

    VkSemaphore image_available{VK_NULL_HANDLE}, render_finished{VK_NULL_HANDLE};

    try {
        vk_result = vkCreateSemaphore(vlk_device, &create_info, nullptr, &image_available);
        assert_eq(vk_result, VK_SUCCESS, "failed to create semafore [image available]");

        vk_result = vkCreateSemaphore(vlk_device, &create_info, nullptr, &render_finished);
        assert_eq(vk_result, VK_SUCCESS, "failed to create semafore [render finished]");
    } catch(std::exception const& ) {
        if(image_available != VK_NULL_HANDLE) vkDestroySemaphore(vlk_device, image_available, nullptr);
        if(render_finished != VK_NULL_HANDLE) vkDestroySemaphore(vlk_device, render_finished, nullptr);
        throw;
    }

    vlk_image_available_sem = image_available;
    vlk_render_finished_sem = render_finished;
}

auto App::vlk_destroy_semaphores() -> void {
    vkDestroySemaphore(vlk_device, vlk_image_available_sem, nullptr);
    vkDestroySemaphore(vlk_device, vlk_render_finished_sem, nullptr);
}
