import { promises as fs } from 'fs'
import { dirname, resolve, isAbsolute } from 'path'
import { ErrorHandler, ErrorCategory, ErrorSeverity } from '@utils/error/error-handler'

export interface WriteOptions {
	createDirectories?: boolean
	overwrite?: boolean
	encoding?: BufferEncoding
	backup?: boolean
}

export interface WriteResult {
	success: boolean
	path: string
	bytesWritten: number
	backupPath?: string
}

export class FileWriter {
	private errorHandler: ErrorHandler

	constructor(errorHandler: ErrorHandler = ErrorHandler.getInstance()) {
		this.errorHandler = errorHandler
	}

	async writeSchema(content: string, outputPath: string, options: WriteOptions = {}): Promise<WriteResult> {
		const resolvedPath = this.resolvePath(outputPath)
		const finalOptions = this.normalizeOptions(options)

		await this.validateWriteOperation(resolvedPath, finalOptions)

		let backupPath: string | undefined
		if (finalOptions.backup && (await this.fileExists(resolvedPath))) {
			backupPath = await this.createBackup(resolvedPath)
		}

		if (finalOptions.createDirectories) {
			await this.ensureDirectoryExists(dirname(resolvedPath))
		}

		await fs.writeFile(resolvedPath, content, { encoding: finalOptions.encoding })
		const stats = await fs.stat(resolvedPath)

		return {
			success: true,
			path: resolvedPath,
			bytesWritten: stats.size,
			backupPath,
		}
	}

	async writeMultipleFiles(files: Array<{ path: string; content: string }>, options: WriteOptions = {}): Promise<WriteResult[]> {
		const results: WriteResult[] = []
		const finalOptions = this.normalizeOptions(options)

		for (const file of files) {
			let result: WriteResult
			try {
				result = await this.writeSchema(file.content, file.path, finalOptions)
			} catch (error) {
				result = this.createFailedResult(file.path, error)
			}
			results.push(result)
		}

		return results
	}

	async ensureDirectoryExists(dirPath: string): Promise<void> {
		const directoryExists = await this.directoryExistsInternal(dirPath)
		if (!directoryExists) {
			await fs.mkdir(dirPath, { recursive: true })
		}
	}

	private async directoryExistsInternal(dirPath: string): Promise<boolean> {
		try {
			await fs.access(dirPath)
			return true
		} catch {
			return false
		}
	}

	async fileExists(filePath: string): Promise<boolean> {
		try {
			await fs.access(filePath)
			return true
		} catch {
			return false
		}
	}

	async createBackup(filePath: string): Promise<string> {
		const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
		const backupPath = `${filePath}.backup.${timestamp}`

		await fs.copyFile(filePath, backupPath)
		return backupPath
	}

	private resolvePath(outputPath: string): string {
		if (!outputPath || outputPath.trim() === '') {
			throw new Error('Output path cannot be empty')
		}

		return isAbsolute(outputPath) ? outputPath : resolve(process.cwd(), outputPath)
	}

	private normalizeOptions(options: WriteOptions): Required<WriteOptions> {
		return {
			createDirectories: options.createDirectories ?? true,
			overwrite: options.overwrite ?? true,
			encoding: options.encoding ?? 'utf8',
			backup: options.backup ?? false,
		}
	}

	private async validateWriteOperation(filePath: string, options: Required<WriteOptions>): Promise<void> {
		const fileExists = await this.fileExists(filePath)

		if (fileExists && !options.overwrite) {
			throw this.errorHandler.createError(`File already exists and overwrite is disabled: ${filePath}`, ErrorCategory.FILE, ErrorSeverity.ERROR, { filePath, overwrite: options.overwrite }, [
				'Set overwrite option to true',
				'Choose a different output path',
				'Remove the existing file manually',
			])
		}

		const directory = dirname(filePath)
		const directoryExists = await this.fileExists(directory)

		if (!directoryExists && !options.createDirectories) {
			throw this.errorHandler.createError(
				`Directory does not exist and createDirectories is disabled: ${directory}`,
				ErrorCategory.FILE,
				ErrorSeverity.ERROR,
				{ directory, createDirectories: options.createDirectories },
				['Set createDirectories option to true', 'Create the directory manually', 'Use an existing directory path']
			)
		}
	}

	private async attemptFallbackWrite(originalPath: string, content: string, options: Required<WriteOptions>): Promise<WriteResult> {
		const fallbackPaths = this.generateFallbackPaths(originalPath)

		for (const fallbackPath of fallbackPaths) {
			const result = await this.tryWriteToPath(fallbackPath, content, options)
			if (result.success) {
				return result
			}
		}

		return this.createFailedResult(originalPath, new Error('All fallback attempts failed'))
	}

	private async tryWriteToPath(filePath: string, content: string, options: Required<WriteOptions>): Promise<WriteResult> {
		try {
			if (options.createDirectories) {
				await this.ensureDirectoryExists(dirname(filePath))
			}

			await fs.writeFile(filePath, content, { encoding: options.encoding })
			const stats = await fs.stat(filePath)

			return {
				success: true,
				path: filePath,
				bytesWritten: stats.size,
			}
		} catch {
			return {
				success: false,
				path: filePath,
				bytesWritten: 0,
			}
		}
	}

	private generateFallbackPaths(originalPath: string): string[] {
		const ext = originalPath.split('.').pop() || ''
		const baseName = originalPath.replace(`.${ext}`, '')
		const timestamp = Date.now()

		return [`${baseName}.fallback.${ext}`, `${baseName}.${timestamp}.${ext}`, resolve(process.cwd(), `fallback-output.${ext}`), resolve(process.cwd(), `output-${timestamp}.${ext}`)]
	}

	private createFailedResult(filePath: string, error: unknown): WriteResult {
		return {
			success: false,
			path: filePath,
			bytesWritten: 0,
		}
	}

	static create(): FileWriter {
		return new FileWriter()
	}
}

export default FileWriter.create()
