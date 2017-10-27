import { NgModule } from '@angular/core';  
import { MyComponent } from '@angular/forms';

/** Some comment */

const basketHeaderBottomModule = angular.module('basketHeaderBottom', [
    basketModule
])
    .component('basketHeaderBottomComponent', basketHeaderBottomComponent)
    .name;

export {MyComponent2}
