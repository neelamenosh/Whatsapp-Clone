'use client';

import { useState, useRef, useCallback, useEffect } from 'react';
import { cn } from '@/lib/utils';

interface IOSSliderProps {
    value: number;
    onChange: (value: number) => void;
    min?: number;
    max?: number;
    step?: number;
    disabled?: boolean;
    className?: string;
    leftIcon?: React.ReactNode;
    rightIcon?: React.ReactNode;
}

export function IOSSlider({
    value,
    onChange,
    min = 0,
    max = 100,
    step = 1,
    disabled = false,
    className,
    leftIcon,
    rightIcon,
}: IOSSliderProps) {
    const trackRef = useRef<HTMLDivElement>(null);
    const [isDragging, setIsDragging] = useState(false);

    // Calculate percentage for fill and thumb position
    const percentage = ((value - min) / (max - min)) * 100;

    const updateValue = useCallback(
        (clientX: number) => {
            if (!trackRef.current || disabled) return;

            const rect = trackRef.current.getBoundingClientRect();
            const position = (clientX - rect.left) / rect.width;
            const clampedPosition = Math.max(0, Math.min(1, position));

            let newValue = min + clampedPosition * (max - min);

            // Snap to step
            newValue = Math.round(newValue / step) * step;
            newValue = Math.max(min, Math.min(max, newValue));

            onChange(newValue);
        },
        [min, max, step, onChange, disabled]
    );

    const handleMouseDown = useCallback(
        (e: React.MouseEvent) => {
            if (disabled) return;
            setIsDragging(true);
            updateValue(e.clientX);
        },
        [updateValue, disabled]
    );

    const handleTouchStart = useCallback(
        (e: React.TouchEvent) => {
            if (disabled) return;
            setIsDragging(true);
            updateValue(e.touches[0].clientX);
        },
        [updateValue, disabled]
    );

    useEffect(() => {
        if (!isDragging) return;

        const handleMouseMove = (e: MouseEvent) => {
            updateValue(e.clientX);
        };

        const handleTouchMove = (e: TouchEvent) => {
            updateValue(e.touches[0].clientX);
        };

        const handleEnd = () => {
            setIsDragging(false);
        };

        window.addEventListener('mousemove', handleMouseMove);
        window.addEventListener('mouseup', handleEnd);
        window.addEventListener('touchmove', handleTouchMove);
        window.addEventListener('touchend', handleEnd);

        return () => {
            window.removeEventListener('mousemove', handleMouseMove);
            window.removeEventListener('mouseup', handleEnd);
            window.removeEventListener('touchmove', handleTouchMove);
            window.removeEventListener('touchend', handleEnd);
        };
    }, [isDragging, updateValue]);

    return (
        <div
            className={cn(
                'flex items-center gap-3 w-full',
                disabled && 'opacity-50 cursor-not-allowed',
                className
            )}
        >
            {/* Left Icon */}
            {leftIcon && (
                <div className="text-muted-foreground shrink-0">
                    {leftIcon}
                </div>
            )}

            {/* Slider Track */}
            <div
                ref={trackRef}
                className={cn(
                    'ios-slider relative flex-1',
                    !disabled && 'cursor-pointer'
                )}
                onMouseDown={handleMouseDown}
                onTouchStart={handleTouchStart}
            >
                {/* Track Background */}
                <div className="ios-slider-track">
                    {/* Filled Portion */}
                    <div
                        className="ios-slider-fill"
                        style={{ width: `${percentage}%` }}
                    />
                </div>

                {/* Thumb */}
                <div
                    className={cn(
                        'ios-slider-thumb',
                        isDragging && 'scale-110'
                    )}
                    style={{ left: `${percentage}%` }}
                />
            </div>

            {/* Right Icon */}
            {rightIcon && (
                <div className="text-muted-foreground shrink-0">
                    {rightIcon}
                </div>
            )}
        </div>
    );
}
