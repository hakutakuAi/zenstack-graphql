import { promises as fs } from 'fs'
import { dirname, resolve, isAbsolute } from 'path'
import { ErrorCategory, PluginError } from '@utils/error'

export interface WriteOptions {
	createDirectories?: boolean
	overwrite?: boolean
	encoding?: BufferEncoding
	backup?: boolean
}

export interface WriteResult {
	path: string
	bytesWritten: number
	backupPath?: string
}

export class FileWriter {
	async writeSchema(content: string, outputPath: string, options: WriteOptions = {}): Promise<WriteResult> {
		try {
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
				path: resolvedPath,
				bytesWritten: stats.size,
				backupPath,
			}
		} catch (error) {
			if (error instanceof PluginError) {
				throw error
			}
			throw new PluginError(`Failed to write file: ${outputPath}`, ErrorCategory.FILE, { path: outputPath, error })
		}
	}

	async ensureDirectoryExists(dirPath: string): Promise<void> {
		const directoryExists = await this.directoryExistsInternal(dirPath)
		if (!directoryExists) {
			try {
				await fs.mkdir(dirPath, { recursive: true })
			} catch (error) {
				throw new PluginError(`Failed to create directory: ${dirPath}`, ErrorCategory.FILE, { path: dirPath, error })
			}
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
		try {
			const timestamp = new Date().toISOString().replace(/[:.]/g, '-')
			const backupPath = `${filePath}.backup.${timestamp}`

			await fs.copyFile(filePath, backupPath)
			return backupPath
		} catch (error) {
			throw new PluginError(`Failed to create backup of file: ${filePath}`, ErrorCategory.FILE, { path: filePath, error })
		}
	}

	private resolvePath(outputPath: string): string {
		if (!outputPath || outputPath.trim() === '') {
			throw new PluginError('Output path cannot be empty', ErrorCategory.FILE, { outputPath })
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
			throw new PluginError(`File already exists and overwrite is disabled: ${filePath}`, ErrorCategory.FILE, { filePath, overwrite: options.overwrite }, [
				'Set overwrite option to true',
				'Choose a different output path',
				'Remove the existing file manually',
			])
		}

		const directory = dirname(filePath)
		const directoryExists = await this.directoryExistsInternal(directory)

		if (!directoryExists && !options.createDirectories) {
			throw new PluginError(`Directory does not exist and createDirectories is disabled: ${directory}`, ErrorCategory.FILE, { directory, createDirectories: options.createDirectories }, [
				'Set createDirectories option to true',
				'Create the directory manually',
				'Use an existing directory path',
			])
		}
	}

	static create(): FileWriter {
		return new FileWriter()
	}
}
